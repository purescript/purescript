* Allow instances that require `Fail` to be empty

  A class instance declaration that has `Prim.TypeError.Fail` as a constraint
  will never be used. In light of this, such instances are now allowed to have
  empty bodies even if the class has members.

  (Such instances are still allowed to declare all of their members, and it is
  still an error to specify some but not all members.)
