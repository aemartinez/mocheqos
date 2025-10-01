## Installation

MoCheQoS can be installed in two ways. Choose the method that best fits your needs:

- **Method 1: Manual Installation** - Requires manual dependency management
- **Method 2: Docker Installation** - Complete isolated environment

While the Docker installation is fully automated, the initial build process can take considerable time.

## Method 1: Manual Installation

### Prerequisites

Before installing MoCheQoS, you need to install Haskell and Cabal. The recommended way is to use GHCup, which provides an easy way to install and manage Haskell toolchain components.

### Installing Haskell and Cabal with GHCup

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

3. **Install Alex and Happy:**
   ```bash
   cabal install alex happy
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
   happy --version
   z3 --version
   ```

### Installing MoCheQoS

Once you have all the prerequisites installed, you can install MoCheQoS:

1. **Clone the repository:**
   ```bash
   git clone git@github.com:aemartinez/mocheqos.git
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

---

## Method 2: Docker Installation

This method provides a complete isolated environment with all dependencies pre-installed and MoCheQoS pre-compiled.

### Prerequisites

- Docker installed on your system
- Sufficient disk space (the image will be ~5 GB)

### Building and Running MoCheQoS with Docker

1. **Clone the repository:**
   ```bash
   git clone git@github.com:aemartinez/mocheqos.git
   cd mocheqos
   ```

2. **Build the Docker image:**
   ```bash
   docker build -t mocheqos .
   ```
   
   **Note:** If you're on Apple Silicon (M1/M2) Mac, you may need to specify the platform `linux/amd64` for compatibility:
   ```bash
   docker build --platform linux/amd64 -t mocheqos .
   ```

3. **Run the container:**
   ```bash
   docker run -v $(pwd)/experiments:/mocheqos/experiments -v $(pwd)/wiki:/mocheqos/wiki -it mocheqos
   ```
   
   This will start an interactive shell inside the container.

4. **Verify MoCheQoS installation (from inside the container):**
   ```bash
   mocheqos --help
   ```

The Docker container includes:
- All required Haskell tools (GHC, Cabal, Alex, Happy)
- Z3 SMT solver
- Python dependencies for experiments
- Pre-configured environment
- All MoCheQoS dependencies pre-installed
- MoCheQoS pre-compiled and ready to use