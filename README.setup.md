## Setting up the dev environment

## Setup scala

The following are steps for **Ubuntu OS (18.04)**. Similar steps should work for other Unix based systems.

```
sudo apt-get update
```

### Install sdk manager

```
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk version
```

### Install Java

- check is Java 11 installed:

```
java --version
```

If Java is not installed:

```
sdk list java
sdk install java 11.0.4.hs-adpt
```

## Install Scala

```
sdk i scala 2.12.8
scala -version
```

Ensure the version is as expected.

### Install sbt

```
sdk install sbt 1.2.8
sbt sbtVersion
```

Ensure the version is as expected.

### Install intelliJ IDE

```
sudo snap install intellij-idea-community --classic
```

## References

- [intelliJ](https://itsfoss.com/install-intellij-ubuntu-linux/)
- [sbt](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Linux.html)
- [Scala](https://www.scala-lang.org/download/)
- [sdkman](https://sdkman.io/install)

### Known setup issues

#### sbt: switching scala or sbt versions requires clearing out 'target' folders
- see the `clean.sh` script (in week 1)
