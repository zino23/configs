#!/bin/bash

## PlantUML
echo "Installing PlantUML..."

# Install dependency java 
if ! which java &> /dev/null; then
    echo "Installing java with Homebrew..."
    brew install java
fi

if ! java -version &> /dev/null; then
    # Depend on standard Homebrew path
    sudo ln -sfn /usr/local/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk
fi

# Install dependency graphviz
if ! which dot &> /dev/null; then
    echo "Installling graphviz with Homebrew..."
    brew install graphviz
fi

# Finally, install PlantUML
if ! which plantuml &> /dev/null; then
    echo "Installling PlantUML with Homebrew..."
    brew install plantuml
fi

echo "PlantUML successfully installed!"
