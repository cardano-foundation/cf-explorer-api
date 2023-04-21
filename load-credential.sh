#!/bin/bash

echo $USERNAME_GITHUB
echo $TOKEN_GITHUB

sed -i "s/{username_github}/$USERNAME_GITHUB/g" .m2/settings.default.xml
sed -i "s/{token_github}/$TOKEN_GITHUB/g" .m2/settings.default.xml
