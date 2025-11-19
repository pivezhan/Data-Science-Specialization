#!/bin/bash

# Script to remove nested .git directories that are causing warnings

echo "Fixing embedded git repositories..."
echo ""

# List of embedded repositories identified from the warning
NESTED_REPOS=(
    "02_RProgramming/project2/ProgrammingAssignment2"
    "04_ExploratoryAnalysis/project1/ExData_Plotting1"
    "05_ReproducibleResearch/programming1/RepData_PeerAssessment1"
    "09_DevelopingDataProducts/final project/exdatapro"
)

# First, remove them from the git index
echo "Step 1: Removing nested repos from git index..."
for repo in "${NESTED_REPOS[@]}"; do
    if [ -d "$repo" ]; then
        echo "  Removing from index: $repo"
        git rm --cached "$repo" 2>/dev/null || true
    fi
done

echo ""
echo "Step 2: Removing .git directories from nested repos..."
# Remove the .git directories from these folders
for repo in "${NESTED_REPOS[@]}"; do
    if [ -d "$repo/.git" ]; then
        echo "  Removing .git directory: $repo/.git"
        rm -rf "$repo/.git"
    else
        echo "  No .git directory found in: $repo"
    fi
done

echo ""
echo "Step 3: Re-adding the folders as regular directories..."
# Now add them back as regular directories
for repo in "${NESTED_REPOS[@]}"; do
    if [ -d "$repo" ]; then
        echo "  Adding: $repo"
        git add "$repo"
    fi
done

echo ""
echo "✅ Done! The nested repositories have been converted to regular folders."
echo ""
echo "You can now run:"
echo "  git add ."
echo "  git commit -m 'Add all course materials'"
echo ""
