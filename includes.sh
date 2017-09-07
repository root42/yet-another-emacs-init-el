#!/bin/bash

find . -name '*.vcxproj' -print0 | xargs -0 egrep '<AdditionalIncludeDirectories>' | sed -E 's/\r\n?|\n/\n/g' | while read -r LINE
do
    PROJECT=$(dirname "$(echo "${LINE}" | cut -d ':' -f 1)")
    BASEPATH=$(readlink -f "${PROJECT}")
    RAW_INCLUDES=$(echo "${LINE}" | cut -d ':' -f 2 | sed -E 's/ *<\/?AdditionalIncludeDirectories>//g')
    echo "${RAW_INCLUDES}" | sed 's/;/\n/g' | sed 's/\\/\//g' | sort | uniq | egrep -v '^[%$]' |
        while read -r INCLUDE
        do
            readlink -f "${BASEPATH}/${INCLUDE}"
        done
done | sort | uniq
