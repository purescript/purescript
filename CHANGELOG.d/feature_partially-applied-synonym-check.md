* Check for partially applied syns in kinds, ctors

  This check doesn't prevent any programs from compiling; it just makes
  sure that a more specific PartiallyAppliedSynonym error is raised
  instead of a KindsDoNotUnify error, which could be interpreted as
  implying that a partially applied synonym has a valid kind and would be
  supported elsewhere if that kind is expected.
