# NED: TODO and in DEVELOPMENT

Compiler: https://github.com/gmnevton/NitroPascal

Code Editor: https://github.com/gmnevton/NitroPascal/tree/main/ide/ned

---

## In Development

- Editor backbuffer class (editor backend engine) - already done; fixes only

- Visual editor component from scratch (~~TSynEdit component rework~~)
  
  - [ ] Keyboard input handling
    
    - [ ] DELETE key
    
    - [ ] BACKSPACE key
    
    - [ ] TAB key
  
  - [x] Mouse click sets cursor location (and right click also)
  
  - [ ] Render scroll bars
  
  - [ ] Handle scrolling
  
  - [ ] Handle selection
  
  - [ ] Handle code highlighting
  
  - [ ] Fully integrate with application
  
  - [ ] Test multi-edit/view on a single document

## TODO

- Save file dialog / folder view

## LATER

- Open file dialog / folder view:
  
  - [ ] Implement showing date/time and/or size for file entries, and only date/time for sub-directories
  
  - [ ] Implement file list entries sort options (later)
  
  - [ ] Implement hide selection feature (later)
  
  - [ ] Speed up lists loading (later)
  
  - [ ] Implement smooth scroll (later)
  
  - [x] (left pane) Selection of folder changes files list
  
  - [x] (right pane) Double click on file list entry opens a sub-directory or selects file, depending on what entry was double clicked
  
  - [x] Implement UpDirectory push button event
  
  - [x] Fix list drawing when list is focused
  
  - [x] Fix list drawing when list is not focused
  
  - [x] Fix list scrolling by keyboard
  
  - [x] Fix OnMouseEnter / OnMouseLeave events
  
  - [x] Speed up lists scrolling
  
  - [x] Custom paint scrollbar
  
  - [x] Fix custom painted scrollbar flickering
  
  - [x] Implement Windows 10 like scrollbar rendering system
