% Prolog program
likes(alice, chocolate).
likes(bob, ice_cream).
likes(charlie, cake).
likes(david, pizza).

friend(alice, bob).
friend(bob, charlie).
friend(charlie, david).

% Example goals
% Query: likes(alice, chocolate).
% Expected Output: true

% Query: likes(alice, pizza).
% Expected Output: false

% Query: friend(alice, bob).
% Expected Output: true

% Query: friend(alice, charlie).
% Expected Output: false
