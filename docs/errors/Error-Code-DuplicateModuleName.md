Generally if this happens it is because you are importing the same module twice. If you get this when opening `psci`, check your `.psci` file and look for duplicate imports. In particular, you might find something like

    :m src/Data/Moment/Simple.purs
    ...
    :l src/Data/Moment/Simple.purs

