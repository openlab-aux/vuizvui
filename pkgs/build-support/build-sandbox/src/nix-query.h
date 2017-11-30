struct query_state;

struct query_state *new_query(void);
void free_query(struct query_state *qs);
bool query_requisites(struct query_state *qs, const char *path);
const char *next_query_result(struct query_state *qs);
