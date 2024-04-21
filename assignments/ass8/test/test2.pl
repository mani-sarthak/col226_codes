% Prolog program
parent(amy, bob).
parent(bob, charlie).
parent(bob, dylan).
parent(charlie, emily).
parent(charlie, felix).
parent(dylan, grace).
parent(dylan, hannah).
parent(emily, isabel).
parent(emily, jack).

male(bob).
male(charlie).
male(dylan).
male(felix).
male(grace).
male(jack).

female(amy).
female(emily).
female(hannah).
female(isabel).

% Example goals
% Query: parent(bob, charlie).
% Expected Output: true

% Query: parent(amy, hannah).
% Expected Output: false

% Query: male(bob).
% Expected Output: true

% Query: female(jack).
% Expected Output: false
