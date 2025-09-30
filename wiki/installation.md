# Installation

The artifact is distributed as a Docker image with all the necessary dependencies and files needed to reproduce the experiments presented in the paper. The Docker image contains the tool compiled and ready to use.

## Load and run docker image

Load the docker image `mocheqos-docker-image.tar` with the following command

```bash
docker load < mocheqos-docker-image.tar
```

After going to the directory in your machine were you want the experiment data to be written to,
create and run a new docker container with the following command

```bash
docker run -v $(pwd)/experiments:/mocheqos/experiments -v $(pwd)/wiki:/mocheqos/wiki -it mocheqos
```

Besides starting the container and giving you a shell within it, this command will bind the `experiments/` and `wiki/` directories within the artifact to local directories in your machine. This will allow you to read and inspect both the wiki and the experiment data and results from your local machine.

Continue the rest of the tutorial ([Usage](usage.md) and [Experiments](experiments.md)) from within the container.

## (OPTIONAL) Build the docker image yourself

Follow this intructions if you want or need to build the docker image. You will need internet connection.

Run the following command within the `code/` directory (or at the top level of the repository if you cloned it from [here](https://bitbucket.org/aemartinez/chorgram/src/mocheqos-fm24/)).

```bash
docker build -t mocheqos .
```

This will create a new image with the tag mocheqos. The process of creating the image retrieves all the necessary dependencies, compiles and install the tool. You can run this newly created image by using the `docker run` command stated above.