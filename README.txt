                                Emonad
                                ======

Author: Andrew Braunstein <bandrew> and Rafe Kettler <rkettler>
Date: 2012-11-04 Sun


Table of Contents
=================
1 Description
2 Architecture
    2.1 Rope
    2.2 Buffer
    2.3 BufferList
    2.4 Editor
    2.5 UI
3 Additional Libraries
    3.1 Rope
    3.2 Vty
    3.3 mtl
    3.4 directory
    3.5 HUnit
    3.6 QuickCheck
4 Installation instructions


1 Description 
--------------
  Emonad is a text editor that is similar to emacs, but written in
  Haskell. The frontend will be text only. The editor logic will be
  completely pure reducing bugs and complications. This will be an
  interesting design approach to text editors since most text editors,
  including emacs, are written with lots of mutable state. While emacs has
  elisp at its core, emonad will have Haskell at its core. Config
  files will be written in Haskell.

2 Architecture 
---------------

2.1 Rope 
=========
   This is the data structure that holds the text of the buffer. We
   extended the rope library with a few extra functions that make it
   easier to use.
   - log time insertion and deletion
   - constant time length

2.2 Buffer 
===========
   This is the data structure that deals with the buffer
   - Move the mark and point
   - Manipulate text
   - Save to a file
   - Load from a file
   - Paging

2.3 BufferList 
===============
   This is the module that deals with the collection of buffers.
   - Add new buffers
   - Kill buffer
   - Switch between buffers
   - Transform the current buffer

2.4 Editor 
===========
   This is the current state of the editor. It knows about the buffer
   list, the context of key presses, the minibuffer, and the terminal
   - Deals with user input
   - Switches between buffer and minibuffer
   - Reads user input (IO)
   - Sets the text in the minibuffer

2.5 UI 
=======
   This is the code that draws the editor in the terminal
   - Primarily uses the vty library
   - Uses combinators to layout the page

3 Additional Libraries 
-----------------------

3.1 Rope 
=========
   This is the efficient data structure that we use to store text

3.2 Vty 
========
   Terminal interaction library

3.3 mtl 
========
   Monads!!!

3.4 directory 
==============
   Library that deals with the filesystem

3.5 HUnit 
==========
   Unit testing

3.6 QuickCheck 
===============
   Property based testing

4 Installation instructions 
----------------------------


  cabal configure
  cabal install

