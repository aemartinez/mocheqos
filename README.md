MoCheQoS: A Tool for Static Analysis of QoS in Communicating Systems

## Installation

### Prerequisites

Before installing MoCheQoS, you need to install Haskell and Cabal. The recommended way is to use GHCup, which provides an easy way to install and manage Haskell toolchain components.

#### Installing Haskell and Cabal with GHCup

1. **Install GHCup:**
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```
   
   Follow the prompts to add GHCup to your PATH. You may need to restart your terminal or run:
   ```bash
   source ~/.ghcup/env
   ```
   
   *For the most up-to-date installation instructions, see the [GHCup website](https://www.haskell.org/ghcup/).*

2. **Install GHC (Glasgow Haskell Compiler) and Cabal:**
   ```bash
   ghcup install ghc
   ghcup install cabal
   ```

3. **Install Alex:**
   ```bash
   cabal install alex
   ```

4. **Install Z3 SMT Solver:**
   
   **On macOS:**
   ```bash
   brew install z3
   ```
   
   **On Ubuntu/Debian:**
   ```bash
   sudo apt-get install z3
   ```
   
   **On other systems:**
   Download from [Z3 releases](https://github.com/Z3Prover/z3/releases) or build from source.

5. **Verify installation:**
   ```bash
   ghc --version
   cabal --version
   alex --version
   z3 --version
   ```

### Installing MoCheQoS

Once you have all the prerequisites installed, you can install MoCheQoS:

1. **Clone the repository:**
   ```bash
   git clone <repository-url>
   cd mocheqos
   ```

2. **Build and install:**
   ```bash
   cabal install
   ```

3. **Verify MoCheQoS installation:**
   ```bash
   mocheqos --help
   ```

