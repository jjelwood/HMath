-- Knuth-Bendix completion algorithm
-- Suppose we have a list of rewrite rules, for example:
--    - x + 0 = x
--    - 0 + x = x
--    - x + -x = 0
--    - (x + y) + z = x + (y + z)
-- Knuth-Bendix completion is an algorithm that takes a set of these rules 
-- and can determine whether two expressions are equivalent under these rules.
-- 
-- Valid equations may be proved, but invalid equations may not necessarily be disproved.
--
-- Terminology:
-- - Order of complexity
-- KB needs to know how to compare the complexity of two expressions. This isn't a formal
-- definition and can be implemented in many ways. It can be though of as the 'length' of the expression.
-- 
-- 



module Core.KnuthBendixCompletion where

