#!/bin/sh

# Generate DNA prefix for Gene Table.
#
# Usage:
#  $ genetable.sh <page number>
#

GeneTable=42
page_no=$1

echo "(?IFPCFFP)!\\\\len(\\\\num(${GeneTable})) \
      (?IFPFIP)!\\len(\\\\num(${page_no})) \
      |-> \
      {0_0}\\\\quote(\\\\num(${GeneTable})) \
      {1_0}\\\\quote(\\\\num(${page_no}))" \
  | runhaskell tools/Compiler.hs
