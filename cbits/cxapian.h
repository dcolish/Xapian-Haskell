#ifndef XAPIAN_H
#define XAPIAN_H

typedef struct _xapian_database xapian_database_t;
typedef struct _xapian_document xapian_document_t;
typedef struct _xapian_enquire xapian_enquire_t;
typedef struct _xapian_query xapian_query_t;
typedef struct _xapian_msets xapian_msets_t;
typedef struct _xapian_terms xapian_terms_t;
typedef struct _xapian_stem xapian_stem_t;

extern "C" {

  xapian_database_t *
  xapian_writable_db_new (const char *filename, int action,
                          const char **error);

  unsigned int
  xapian_writable_db_add_document(xapian_database_t *database,
                                  xapian_document_t *document);

  xapian_database_t *
  xapian_database_new (const char *cFilename, const char **errorStr);

  void
  xapian_database_delete (xapian_database_t *database);

  xapian_document_t *
  xapian_document_new ();

  xapian_document_t *
  xapian_get_document (xapian_database_t *database, int doc_id, const char **errorStr);

  void
  xapian_document_delete (xapian_document_t *document);

  void
  xapian_document_set_data (xapian_document_t *document, const char* data);

  const char *
  xapian_document_get_data (xapian_document_t *document);

  void
  xapian_document_add_term(xapian_document_t *doc, const char* term);

  void
  xapian_document_add_posting (xapian_document_t *document,
                               const char* posting, int pos);

  void
  xapian_document_add_value (xapian_document_t *document, int valno,
                             const char *value);

  xapian_enquire_t *
  xapian_enquire_new (xapian_database_t *database);

  void
  xapian_enquire_delete (xapian_enquire_t *enquire);

  xapian_query_t *
  xapian_query_empty ();

  xapian_query_t *
  xapian_query_new (const char* term);

  xapian_query_t *
  xapian_query_combine (int op, xapian_query_t *vqa, xapian_query_t *vqb);

  xapian_query_t *
  xapian_query_new_value (int op, int valno, const char *value);

  xapian_query_t *
  xapian_query_new_double (int op, xapian_query_t *q, double d);

  const char *
  xapian_query_describe (xapian_query_t *vqa);

  void
  xapian_query_delete (xapian_query_t *query);

  xapian_msets_t *
  xapian_enquire_query (xapian_enquire_t* enquire, xapian_query_t *query,
                        int min, int max);

  int
  xapian_msets_valid (xapian_msets_t *msets);

  int
  xapian_msets_get (xapian_msets_t *msets);

  void
  xapian_msets_next(xapian_msets_t *msets);

  xapian_terms_t *
  xapian_get_terms (_xapian_document* document);

  int
  xapian_terms_valid (xapian_terms_t *terms);

  const char *
  xapian_terms_get (xapian_terms_t *terms);

  void
  xapian_terms_next(xapian_terms_t *terms);

  xapian_stem_t *
  xapian_stem_new(const char *language);

  void
  xapian_stem_delete(xapian_stem_t *stem);

  void
  xapian_stem_string (xapian_stem_t *stem, xapian_document_t *document,
                      const char *string);

  const char *
  xapian_stem_word (xapian_stem_t *stem, const char *word);

  xapian_query_t *
  xapian_parse_query (const char *query_string,
                      xapian_stem_t *stem);
}

#endif