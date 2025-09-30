# Using MoCheQoS

Let's start by analyzing a simple system consisting of the following two machines:

![Two-party example](format-example-sys.png)
where

- Γ_A = (5 ≤ mem ≤ 10) ∧ (cost = 0.2 * mem)
- Γ_B = (mem = 0) ∧ (cost = 1)

The property we'll check is the following: ⟨G⟩(2 ≤ cost ≤ 3), where

- ⟨ ⟩ is the possibility modality,
- G = A -> B: x; B -> A: y; A -> B: z2

## MoCheQoS CLI
The syntax of MoCheQoS CLI is
```bash
mocheqos <cmd > <qcfsms > <prop> <k> [options]
```
where 

- `<cmd>` is the (name of the) operation to apply; it can be either `satisfiability` or `validity` for checking the satisfiability or validity of the property prop on the communicating system `qcfsms`; in the case of `validity`, MoCheQoS will negate `prop` and search for a counterexample,
- `<qcfsms>` is the path to a `.qosfsa` file containing a textual description of the qos-extended system at hand,
- `<prop>` is the path to a `.ql` file containing a textual description of the QL formula,
- `<k>` specifies the bound on the length of the runs to be considered, and
- `[options]` specifies optional requests to MoCheQoS:

    - `--unfoldings <u>` specifies the maximum number of unfoldings of the `*` operators in the g-choreographies in `<prop>` (`u` defaults to `k`.),
    - `--show-model` prints a run that satisfies the property, if any, and
    - `--verbose` executes the analysis in verbose mode, which prints the current length of runs being considered and the total number of runs with such length.

Additionally, the flag `--help` will print a help message.

**Warning**: in the rest of our instructions we assume that these commands are executed from the entry point of the docker container (namely `/mocheqos`).

### 1. Inspect files (optional)
The files `experiments/format-example/sys.qosfsa` and `experiments/format-example/Phi.ql` respectively yield the the description of the qos-extended communicating system and the QL formula above in the syntax of MoCheQoS. 
A detailed explanation of the syntax of these files is described in [this section](syntax.md).

### 2. Check satisfiability
The `satisfiability` subcommand of MoCheQoS takes three parameters: the system to be analized, the property, and the bound on the length of runs to be considered; these parameters are respectively referred as  `.qosfsa`, `.ql`, and `k` in the architecture of MoCheQoS (cf. Section 4 of the paper). The following command checks if the property is satisfiable in the system:
```bash
mocheqos satisfiability experiments/format-example/sys.qosfsa experiments/format-example/Phi.ql 6
```

The output should be:
```
Satisfiable.
```

Since the property is satisfiable, we might be interested in finding a model that satisfies it. To do so, we can use the option `--show-model`:
```bash
mocheqos satisfiability experiments/format-example/sys.qosfsa experiments/format-example/Phi.ql 6 --show-model
```

The output should be:
```
Satisfiable.

Model: A.B!x; A.B?x; B.A!y; B.A?y; A.B!z2; A.B?z2
```

Which exhibits the (trace of a) run `A.B!x; A.B?x; B.A!y; B.A?y; A.B!z2; A.B?z2` to witness the satisfiability of the property `experiments/format-example/Phi.ql`.


### 3. Check validity
As explained in Section 4.2 of the paper, MoCheQoS checks validity of a formula by verifying the satisfiability of the negated formula. In our example, this is done by executing the `validity` subcommand as follows:
```bash
mocheqos validity experiments/format-example/sys.qosfsa experiments/format-example/Phi.ql 6
```

The output should be:
```
No counterexample found within the given bounds.
```

We might be interested in increasing the bound on the length of runs to be considered to find a counterexample.
For instance, if we execute
```bash
mocheqos validity experiments/format-example/sys.qosfsa experiments/format-example/Phi.ql 10
```
(that is, we incresed the paramenter `k` to `10`) the output should be:
```
Not valid.
```

Again, we can use the option `--show-model`, in this case to show the counterexample:
```bash
mocheqos validity experiments/format-example/sys.qosfsa experiments/format-example/Phi.ql 10 --show-model
```

The output should be:
```
Not valid.

Counterexample: A.B!x; A.B?x; B.A!y; B.A?y; A.B!z1; A.B?z1; B.A!y; B.A?y; A.B!z2; A.B?z2
```

This run does not satisfy the property because G, the g-choreography indexing the possibility modality, is never realized in the run. (Recall that G = A -> B: x; B -> A: y; A -> B: z2.)
