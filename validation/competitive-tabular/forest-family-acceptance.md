# Forest acceptance, separate from the selected model

Added on 12 September 2026 before any locked acceptance outcomes were opened.
These requirements add to the original selected-model gates. They prevent a
strong boosted model from concealing an unusable retained forest.

- The selected forest-family configuration must complete its native fit on
  every row of the declared full training pool. An overall primary model from
  another family does not waive this requirement.
- Its complete held-out predictions must survive saving and a fresh R session,
  with a maximum absolute prediction difference of 1e-12.
- Its acceptance regression RMSE must be at most 1.10 times the best completed
  native random-forest reference RMSE at the same thread count.
- Its binary or multiclass log loss must be at most 1.15 times the matched
  native random-forest reference log loss plus 0.002.
- If the native forest reference fails its resource gate, there is no valid
  quality reference and no automatic pass. Keep the failure and resolve the
  operational gap before claiming forest readiness.

The 50,000-row development policy probes diagnose choices of `mtry` and node
size. They do not replace the locked full-training comparison. All protocol
amendments and their hashes must be included in the final acceptance freeze.
