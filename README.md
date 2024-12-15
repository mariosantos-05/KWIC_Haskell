# KWIC_Haskell
This repository implement a KWIC (Key Word in context) on haskell using "The One" as the programming style


This algorithm takes a list of titles or phrases and generates an alphabetically sorted list of all keywords in these titles, along with their surrounding context. KWIC is a classic example of string manipulation and sorting, useful for building indexes and concordances.

---

## Input  
A text file containing a list of titles or phrases, one per line.

## Output  
An alphabetized list of keywords, where each keyword is presented with its surrounding context within the original title.

---

## Algorithm Steps  

1. **Read Input**  
   - Read the input text file and store the titles in a list.

2. **Generate Keyword List**  
   - For each title:
     - Split the title into individual words (keywords).
     - Ignore common words ("stop words") such as `a`, `the`, `is`, `of`, etc. (A predefined list of stop words will be provided).
     - Store each keyword along with its original title.

3. **Circular Shifts**  
   - For each keyword, create a "circularly shifted" version of the title where the keyword appears at the beginning.  
   - Example: For the title `"The quick brown fox"` and the keyword `"brown"`, the circularly shifted version is `"brown fox The quick"`.

4. **Sort**  
   - Sort the list of keywords alphabetically, based on the circularly shifted titles.

5. **Output**  
   - Print the sorted list of keywords and their contexts (circularly shifted titles) to the console or write them to a new text file.

---

## Example  

### Input
```plaintext
The quick brown fox  
A brown cat sat  
The cat is brown
```

### Output
```plaintext
brown cat sat A          (from "A brown cat sat")  
brown fox The quick      (from "The quick brown fox")  
brown is The cat         (from "The cat is brown")  
cat is brown The         (from "The cat is brown")  
cat sat A brown          (from "A brown cat sat")  
fox The quick brown      (from "The quick brown fox")  
quick brown fox The      (from "The quick brown fox")  
```

# Repositório mantido e gerenciado por:
- [Átila Fernandes](https://github.com/At1l4)
- [Lucas Adeodato](https://github.com/HimeakLucas)
- [Mario Santos](https://github.com/mariosantos-05)

