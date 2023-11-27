#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>

#define CHECK(VAL, TYPE) if (!is##TYPE(VAL)) { \
error(#VAL " must be a(n) " #TYPE ".");        \
}

#define CHECKLEN(VAL, TYPE, LEN) if (!is##TYPE(VAL) || length(VAL) != LEN) { \
error(#VAL " -- must be a length -- " #LEN " " #TYPE ".");                   \
}

#define CHECKMATROW(VAL, TYPE, NROW) if (!isMatrix(VAL) || !is##TYPE(VAL) || NUMROWS(VAL) != NROW) { \
error(#VAL " must be a matrix with " #NROW " rows of type " #TYPE ".");                              \
}                                                                                                    \

#define NUMROWS(MAT) (INTEGER(GET_DIM(MAT))[0])
#define NUMCOLS(MAT) (INTEGER(GET_DIM(MAT))[1])

SEXP ldagibbs(SEXP documents,
              SEXP K_,
              SEXP V_,
              SEXP N_,
              SEXP alpha_,
              SEXP eta_,
              SEXP initial_,
              SEXP n_) {
  GetRNGstate();
  long dd;
  int ii;
  int kk;

  CHECK(documents, NewList);
  int nd = length(documents);
  CHECKLEN(K_, Integer, 1);
  int K = INTEGER(K_)[0];
  CHECK(V_, Integer);
  int V = 0;
  for (ii = 0; ii < length(V_); ++ii) {
    V += INTEGER(V_)[ii];
  }
  CHECKLEN(N_, Integer, 1);
  int N = INTEGER(N_)[0];
  CHECKLEN(alpha_, Real, 1);
  double alpha = REAL(alpha_)[0];
  CHECKLEN(eta_, Real, 1);
  double eta = REAL(eta_)[0];
  CHECKLEN(n_, Integer, 1);
  int n_init = INTEGER(n_)[0];

  // init and protect output
  SEXP retval;
  PROTECT(retval = allocVector(VECSXP, 4));

  // initialisiere verschiedene Elemente
  SEXP assignments;
  SEXP topics = NULL;
  SEXP topic_sums = NULL;
  SEXP document_sums;
  SEXP initial = NULL;
  SEXP initial_topic_sums = NULL;
  SEXP initial_topics = NULL;

  SET_VECTOR_ELT(retval, 0, assignments = allocVector(VECSXP, nd));
  if (!assignments) {
    error("Unable to allocate memory for assignments vector");
  }
  SET_VECTOR_ELT(retval, 1, topics = allocMatrix(INTSXP, K, V));
  if (!topics) {
    error("Unable to allocate memory for topic matrix");
  }
  SET_VECTOR_ELT(retval, 2, topic_sums = allocMatrix(INTSXP, K, length(V_)));
  if (!topic_sums) {
    error("Unable to allocate memory for topic sums");
  }
  SET_VECTOR_ELT(retval, 3, document_sums = allocMatrix(INTSXP, K, nd));
  if (!document_sums) {
    error("Unable to allocate memory for document sums");
  }

  if (!isNull(initial_)) {
    CHECK(initial_, NewList);
    SEXP names = getAttrib(initial_, R_NamesSymbol);

    for (ii = 0; ii < length(initial_); ++ii) {
      if (!strcmp(CHAR(STRING_ELT(names, ii)), "assignments")) {
        initial = VECTOR_ELT(initial_, ii);
        CHECKLEN(initial, NewList, nd);
      } else if (!strcmp(CHAR(STRING_ELT(names, ii)), "topic_sums")) {
        initial_topic_sums = VECTOR_ELT(initial_, ii);
        if (!isInteger(initial_topic_sums) ||
            INTEGER(GET_DIM(initial_topic_sums))[0] != K ||
            INTEGER(GET_DIM(initial_topic_sums))[1] != length(V_)) {
          error("Initial topic sums must be a K x length(V) integer matrix.");
        }
      } else if (!strcmp(CHAR(STRING_ELT(names, ii)), "topics")) {
        initial_topics = VECTOR_ELT(initial_, ii);
        if (!isInteger(initial_topics) ||
            INTEGER(GET_DIM(initial_topics))[0] != K ||
            INTEGER(GET_DIM(initial_topics))[1] != V) {
          error("Initial topics (%d x %d) must be a %d x %d integer matrix.",
                INTEGER(GET_DIM(initial_topics))[0],
                                                INTEGER(GET_DIM(initial_topics))[1],
                                                                                K,
                                                                                V);
        }
      } else {
        error("Unrecognized initialization field: '%s'",
              CHAR(STRING_ELT(names, ii)));
      }
    }
  }

  if ((initial_topic_sums == NULL) ^ (initial_topics == NULL)) {
    error("initial topic sums and topics must both be specified.");
  }


  for (ii = 0; ii < K * V; ++ii) {
    INTEGER(topics)[ii] = INTEGER(initial_topics)[ii];
  }
  for (ii = 0; ii < K * length(V_); ++ii) {
    INTEGER(topic_sums)[ii] = INTEGER(initial_topic_sums)[ii];
  }

  for (ii = 0; ii < K * nd; ++ii) {
    INTEGER(document_sums)[ii] = 0;
  }

  for (dd = 0; dd < nd; ++dd) {
    // loop over documents
    int ww;
    SEXP document = VECTOR_ELT(documents, dd);

    // docs: erste Zeile ID, zweite Zeile Count
    CHECKMATROW(document, Integer, 2);

    // number of words per document
    int nw = INTEGER(GET_DIM(document))[1];
    // initialisiere Vektor der Assignments
    SET_VECTOR_ELT(assignments, dd, allocVector(INTSXP, nw));
    // und speichere zusaetzlich in zs
    SEXP zs = VECTOR_ELT(assignments, dd);
    if (!zs) {
      error("Unable to allocate memory for document (%ld) assignments", dd);
    }

    for (ww = 0; ww < nw; ++ww) {
      //loop over words in document
      //id in Zeile 1
      int word = INTEGER(document)[ww * 2];
      //count in Zeile 2 (eigntlich immer = 1)
      int count = INTEGER(document)[ww * 2 + 1];
      if (count < 0) {
        error("Count must be positive.");
      }
      if (word >= V || word < 0) {
        error("Word (%d) must be non-negative and less than "
                "the number of words (%d).", word, V);
      }
      //FRAGE: setzt diese Aenderung von zs nun auch den assignments-Vektor in der Ausgabe?
      // setze alle Assignments auf -1 (= kein Topic)
      INTEGER(zs)[ww] = -1;
    }
  }

  // Was passier hier`? Was macht R_alloc?
  // initialisiere Pointer p
  double* p = (double *)R_alloc(K, sizeof(double));

  // beginne Loop ueber die Iterationen!
  int iteration;
  for (iteration = 0; iteration < N; ++iteration) {
    // Re-Assignment, d.h. Gibbs Sampling startet hier, und zwar jeweils nur ueber
    // alle Dokumente ab n.init + 1
    for (dd = n_init; dd < nd; ++dd) {
      R_CheckUserInterrupt();
      // ziehe aus den assignments das entsprechende Dokument und setze zs auf diese Werte
      SEXP zs = VECTOR_ELT(assignments, dd);
      // ziehe das Dokument selber aus docs
      SEXP document = VECTOR_ELT(documents, dd);
      // initialisiere Zaehler ueber die Woerter ww und setze Anzahl Woerter im Dokument nw
      int ww;
      int nw = INTEGER(GET_DIM(document))[1];
      // initialisiere initial_d dokumentenweise assignments
      SEXP initial_d = NULL;

      if (initial != NULL) {
        initial_d = VECTOR_ELT(initial, dd);
        CHECKLEN(initial_d, Integer, nw);
      }

      // initialisiere verschiedene Variablen per Pointer
      int* topics_p = INTEGER(topics);
      int* topic_sums_p = INTEGER(topic_sums);
      int* document_sums_p = INTEGER(document_sums);
      int* document_p = INTEGER(document);
      int* V_p = INTEGER(V_);
      int V_l = length(V_);
      int* zs_p = INTEGER(zs);

      for (ww = 0; ww < nw; ++ww) {
        // Loop ueber Woerter
        int* z = &zs_p[ww]; // Topiczuordnung fuer Wort
        long word = -1; // initialisiere word ID
        int count = 1; // count ist immer = 1
        int* topic_wk;
        int* topic_k;
        int* document_k;

        word = document_p[ww * 2]; // setze word ID
        long partialsum = 0;
        int topic_index = -1;
        // Loop ueber Vokabular
        // (diese Schleife sollte man durch den folgenden Ausruck ersetzen koennen):
        // topic_index = word
        for (ii = 0; ii < V_l; ++ii) {
          partialsum += V_p[ii]; // identisch mit += 1
          if (word < partialsum) { // partialsum = ii
            topic_index = ii; // setze topic_index auf word ID
          }
        }
        if (topic_index == -1) { // falls word ID hoeher als Anzahl Vokabeln
          error("Oops I did it again");
        }
        count = document_p[ww * 2 + 1]; // setze word Count

        // in allen Iterationen bis auf der ersten, oder?!
        if (*z != -1) {
          topic_wk = &topics_p[(*z) + K * word];
          topic_k = &topic_sums_p[*z + K * topic_index];
          *topic_wk -= count;
          *topic_k -= count;
          document_k = &document_sums_p[K * dd + *z];
          *document_k -= count;

          if (*topic_wk < 0 || *topic_k < 0 || *document_k < 0) {
            error("Counts became negative for word (%ld): (%d, %d, %d)",
                  word, *topic_wk, *topic_k, *document_k);
          }
        }

        double r = unif_rand();
        double p_sum = 0.0;

        if (*z == -1) {
          for (kk = 0; kk < K; ++kk) {
            if (initial != NULL) {
              if (INTEGER(initial_d)[ww] == kk) {
                p[kk] = 1;
              } else {
                p[kk] = 0;
              }
            } else {
              p[kk] = 1;
            }
            p_sum += p[kk];
          }
        } else {
          for (kk = 0; kk < K; ++kk) {
            p[kk] = (document_sums_p[K * dd + kk] + alpha);
            p[kk] *= (topics_p[kk + K * word] + eta);
            p[kk] /= (topic_sums_p[kk + K * topic_index] + V * eta);
            p_sum += p[kk];
          }
        }
        if (p_sum==0) {
          for (kk = 0; kk < K; ++kk) p[kk]=1.0/K;
          REprintf("Warning:Sum of probabilities is zero, assigning equal probabilities.\n");
        }

        *z = -1;
        for (kk = 0; kk < K; ++kk) { // Re-Assignment Strategie:
          // Wann wird r das erste Mal ueberdie kumulierten Wkeiten ueberschritten
          if (r < p[kk] / p_sum) {
            *z = kk;
            break;
          }
          r -= p[kk] / p_sum;
        }

        // sollte nicht vorkommen!!
        if (*z == -1) {
          for (kk = 0; kk < K; ++kk) {
            REprintf("%g\n", p[kk]);
          }
          error("This should not have happened (%g).", r);
        }
        topics_p[*z + K * word] += count;
        topic_sums_p[*z + K * topic_index] += count;
        document_sums_p[K * dd + *z] += count;
      }
    }
  }

  PutRNGstate();
  UNPROTECT(1);
  return retval;
}
