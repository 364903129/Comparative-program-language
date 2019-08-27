#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: bitstream.st,v 1.1 2019-02-01 14:36:25-08 - - $"

"Partner: Haofan Wang   (hwang108@ucsc.edu)
 Partner: Michael Zhang (mzhang62@ucsc.edu)"

execname := '$RCSfile: bitstream.st,v $' =~ '.*: (.*),v.*' at: 1.

nl := Character nl.
priorityQueue := SortedCollection new.


"Bit Stream to read the input file"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]


"FileStream"
FileStream extend [
   |bitIndex currentChar|

   nextBit [
      |bit|
      bitIndex isNil ifTrue: [bitIndex := 0].
      bitIndex = 0 ifTrue: [
         bitIndex := 8.
         currentChar := self next.
      ].
      bit := currentChar value bitAt: bitIndex.
      bitIndex := bitIndex - 1.
      ^ bit
   ]

   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]

]

"write bits"
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]


"leaf"
Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   depthFirst: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: visitor [
      visitor value: char value: self isleaf.
   ]

   isleaf [
      ^true.
   ]

].


"tree"
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   depthFirst: visitor prefix: string [
      left depthFirst: visitor prefix: string, '0'.
      right depthFirst: visitor prefix: string, '1'.
   ]
   
   postorder: visitor [
      left postorder: visitor.
      right postorder: visitor.
      visitor value: char value: self isleaf.
   ]
   left [ ^ left ]
   right [^ right ]

   isleaf [
      ^false.
   ]

].


"-----------freq table------------"
Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

"----------------start to evaluate options--------------------"
Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |

"--------------------------debug----------------------------"
    (opt = $d) ifTrue: [
        "'start debug' displayNl"
    ].

"------------------------print table------------------------"
    (opt = $t) ifTrue: [
        "'print table' displayNl."

"open and read file"
        file := Smalltalk getArgv: 2.
        inputfile := FileStream open: file
                                mode: FileStream read.

"create a freq table"
    table := ZeroArray new: 257.

"iterate the file and put char into the table"
    [inputfile atEnd not] whileTrue: [
        character := inputfile next asInteger.
        "character displayNl."
        table incr: character.
        ].
        table at: 256 put: 1.

"create leafs and put them into the table"
    table keysAndValuesDo: [:key :count |
    (count notNil and: [count > 0]) ifTrue: [
        priorityQueue add: (Leaf new: key count: count).
        "'added leaf' displayNl."
        "priorityQueue displayNl."
        ]
    ].

"remove the two smallest elemnts from the queue, 
combine them until only 1 tree left"
    [(priorityQueue size) > 1] whileTrue: [
                leaf1 := priorityQueue removeFirst.
                leaf2 := priorityQueue removeFirst.
                treeName := leaf1 char.
                treecount := leaf1 count + leaf2 count.
                "treecount displayNl." "display the total count"
                t := Tree new: treeName count: treecount 
                          left: leaf1 right: leaf2.
                priorityQueue add: t.
                "priorityQueue displayNl."
            ].
"create the table to print"
    encodingTable := ZeroArray new: 257.
    tree := priorityQueue first.
    "tree displayNl."
    tree depthFirst: [:character :string |
                encodingTable at: (character asInteger) put: string.
            ] prefix: ''.
    "encodingTable displayNl."

encodingTable keysAndValuesDo: [:key :value |
                (value notNil and: [value > 0]) ifTrue: [
                    |index char count string pad|
                    ((key < 33 or: [key > 126]) 
                        and: [key ~= 256]) ifTrue: [
                        char := key printPaddedWith: Character space
                                    to: 0 base: 16.
                        (char size) = 1 ifTrue: [
                            char := 'x0', char.
                        ] ifFalse: [
                            char := 'x', char.
                        ].
                    ] ifFalse: [
                        key = 256 ifTrue: [
                            char := 'EOF'.
                        ] ifFalse: [
                            char := ' ', key asCharacter asString, ' '.
                            "(char size) displayNl."
                        ].
                    ].

                    "(char size) = 1 ifTrue: [
                                            char := ' ', char, ' '.
                                            char displayNl.
                                        ]."

                    stdout << char << '    '.
                    count := (table at: key) printString.
                    pad := 4 - count size.
                    [pad > 0] whileTrue: [
                        count := ' ',count.
                        pad := pad - 1.
                    ].
                    stdout << count << ' '.
                    string := value.
                    stdout << string << nl.
                ].
            ].

    ].

"----------------------compress-----------------------"
    (opt = $c) ifTrue: [
        "'start compress' displayNl."

        "open and read file"
        file := Smalltalk getArgv: 2.
        inputfile := FileStream open: file
                                mode: FileStream read.
        "if has output file, print to the file.
         else print to the standard output"
        outputname := Smalltalk getArgv: 3.
            (outputname isNil) ifTrue:
                [outputfile := stdout.]
                ifFalse: 
                [outputfile := FileStream open: outputname 
                                          mode: FileStream write
                ].

                "stdout << 'the outputfile is ' << outputfile << nl."

           outBytes := OutBits new: outputfile.

        "open outputfile"   
        "outputfile := FileStream open: outputname."

"create a freq table"
    table := ZeroArray new: 257.

"iterate the file and put char into the table"
    [inputfile atEnd not] whileTrue: [
        character := inputfile next asInteger.
        "character displayNl."
        table incr: character.
        ].
        table at: 256 put: 1.

"create leafs and put them into the table"
    table keysAndValuesDo: [:key :count |
    (count notNil and: [count > 0]) ifTrue: [
        priorityQueue add: (Leaf new: key count: count).
        "'added leaf' displayNl."
        "priorityQueue displayNl."
        ]
    ].

"remove the two smallest elemnts from the queue, 
combine them until only 1 tree left"
    [(priorityQueue size) > 1] whileTrue: [
                |leaf3 leaf4|
                leaf3 := priorityQueue removeFirst.
                leaf4 := priorityQueue removeFirst.
                TreeName := leaf3 char.
                Treecount := leaf3 count + leaf4 count.
                "Treecount displayNl." 
                "display the total count"
                t1 := Tree new: TreeName count: Treecount 
                           left: leaf3 right: leaf4.
                priorityQueue add: t1.
                "priorityQueue displayNl."
            ].



"create the table to print"
    encodingTable := ZeroArray new: 257.
    tree := priorityQueue first.
    "tree displayNl."
    tree depthFirst: [:character :string |
                encodingTable at: (character asInteger) put: string.
            ] prefix: ''.
    "[ZeroArray at:98]  displayNl."

    
    bitCount := 0.
   
    "write encoding table in big-endian by postorder"
    tree postorder: [:char :isleaf|
    "if it's an interior node, write 1 bit"
    isleaf ifFalse: [
      bit := 1 asInteger bitAt: 1.
      outBytes writeBit: bit. 
      "stdout << 'Interior Node: ' <<  bit << nl."
      bitCount := bitCount + 1.
      ]
    "if it's a leaf, write 0 bit and its 8-bit representation"
    ifTrue: [
        outBytes writeBit: 0.
        "stdout << 'Leaf: ' << 0 << nl."
        bitCount :=  bitCount + 1.
       8 to: 1 by: -1 do: [:ind |
       bit := char asInteger bitAt: ind.
       outBytes writeBit: bit .
       "stdout << 'Index: ' << ind << ' || Bit: ' << bit << nl."
       bitCount := bitCount +1.
      ]. 
      "decimalChar := char asInteger."
      "stdout << '---Current char is: ' << char << nl."
      "stdout << '---Decimal is: ' << decimalChar << nl."

      char = 256 ifTrue: [
      outBytes writeBit: 1.
      "stdout << 'Appending 1 for EOF || Bit: 1' << nl."
      bitCount := bitCount + 1.
      ].

      char = 0 ifTrue: [
      outBytes writeBit: 0.
      "stdout << 'Appending 0 for ZERO || Bit: 0' << nl."
      bitCount := bitCount + 1.
      ]
     ]. "end of isLeaf True"
    ]. "end of postorder"

    "completed the tree!"
    outBytes writeBit: 1.
    "stdout << 'Appending 1 for end of TREE || Bit: 1' << nl."
    bitCount := bitCount + 1.

    "stdout << 'Counts ' << bitCount << nl."
    "--------COMPLETED STEP G--------------"
    "stdout << '-----BEGIN ENCODING-----' << nl."
    "reopen the input file to begin encoding "
    inputfile := FileStream open: file
                            mode: FileStream read.

    [inputfile atEnd not] whileTrue: [
      |thisChar|
      thisChar := inputfile next asInteger.
      "stdout << 'Current character is: ' << thisChar << nl."
      "current character's path in the encoding tree"
      charPath := encodingTable at: thisChar.
      "stdout << 'CharPath is: ' << charPath << nl."
      "write this path to the output"
      1 to: charPath size by: 1 do: [:ind |
           "stdout << 'Index: ' << ind << nl."
           currentBit := (charPath at:ind) asInteger.
           currentBit := currentBit -48.
           "outBytes writeBit: currentBit."
           outBytes writeBit: currentBit.
           "stdout << 'Bit: ' << currentBit  << nl."
           bitCount := bitCount + 1.
      ]. 
    ]. "whileTrue ends here"
    "write EOF to output"
    "stdout << 'Writing EOF' << nl."
    charPath2 := encodingTable at: 256.
    1 to: charPath2 size by:1 do: [:ind |
     currentBit2 := (charPath2 at:ind) asInteger.
     currentBit2 := currentBit2 - 48.
     outBytes writeBit: currentBit2.
     "stdout << 'Bit: ' << currentBit2 << nl."
     bitCount := bitCount + 1.
    ].

    "stdout << 'Current bitCount is: ' << bitCount << nl."
    "padding extra 0's"
    (bitCount \\ 8) = 0 ifTrue: ["do nothing"]
    ifFalse: [ 
     |remainder|
     remaidner := 7- (bitCount \\ 8).
     0 to: remaidner do: [:value |
        outBytes writeBit: 0.
        "stdout << 'Padding: 0' << nl."
        bitCount := bitCount +1.
     ].
    ].
   "stdout << 'Current bitCount is: ' << bitCount << nl."
   inputfile close.
   outputfile close.

    ].  "end of compression"

"------------------uncompress----------------------"
    (opt = $u) ifTrue: [
        |visit eof currByte stack node |
        "'start decompress' displayNl."


        file := Smalltalk getArgv: 2.
        infile := FileStream open: file
                                mode: FileStream read.
        "if has output file, print to the file. 
        else print to the standard output"
        outputname := Smalltalk getArgv: 3.
            (outputname isNil) ifTrue:
                [outfile := stdout.]
                ifFalse: 
                [outfile := FileStream open: outputname 
                                          mode: FileStream write.
                ].

           outBytes := OutBits new: outfile.
           inBytes := BitStream new: infile.

        " infilename := Smalltalk getArgv: 2.
                    infile := FileStream open: infilename 
                              mode: FileStream read.
                    outfilename := Smalltalk getArgv: 3.
                    (outfilename isNil not) ifTrue: [
                        outfile := FileStream 
                        open: outfilename mode: write.
                    ]."

            currByte := 0.
            visit := false.

            stack := OrderedCollection new.

            "stdout << 'visit is: ' << visit << nl."
            "stdout << '(infile atBitEnd not) ' 
            << (infile atBitEnd not) << nl."
            "--- Iterate through file bit-by-bit ---"
            [(infile atBitEnd not) & (visit = false)] whileTrue: [
                |CurrentBit|
                "read in one bit"
                "stdout << 'visit is: ' << visit << nl."
                CurrentBit := infile nextBit.
                "stdout << 'Current Bit is: ' << CurrentBit << nl."
                CurrentBit = 0 ifTrue: [
                    currByte := 0.
                    "--- Rebuild Byte ---"
                    "stdout << 'Into Byte Loop' << nl."
                    8 to: 1 by: -1 do: [:index |
                        CurrentBit := infile nextBit.
                        "stdout << 'Current Bit is: ' 
                        << CurrentBit << nl."
                        currByte := currByte bitAt: index
                                    put: CurrentBit.
                    ].
                    "stdout << 'Out of Byte Loop' << nl."
                    currByte = 0 ifTrue: [
                        CurrentBit := infile nextBit.
                        CurrentBit = 1 ifTrue: [
                            "stdout << 'FOUND EOF'  << nl."
                            currByte := 256.
                        ].
                    ].
                    "stdout << 'Current Byte is: ' << currByte << nl."
                    node := Leaf new: currByte asCharacter count: 1.
                    stack addFirst: node.
                ] ifFalse: [ "if the bit is a 1, 
                             pop two trees from the stack"
                    
                    stack size = 1 ifTrue: [
                    "stdout << 'Obtained the decoding tree.' << nl."
                        visit := true.
                        "stdout << 'visit is: ' << visit << nl."
                    ] ifFalse: [

                        node1 := stack removeFirst.
                        node0 := stack removeFirst.
                        tree123 := Tree new: (node0 char)
                        count: 1 left: node0 right: node1.
                        "tree123 displayNl."
                        stack addFirst: tree123.
                        "tree123 displayNl."
                    ].
                ].
            ].
            "stdout << 'Exit from the visit loop' << nl."
            root := stack removeFirst.
            "stdout << root << nl."
            node := root. "pointer to the root of the tree"
            eof := false.
            [(infile atBitEnd not) & (eof = false)] whileTrue: [
                |readbit|
                "stdout << 'EOF is: ' << eof << nl."
                readbit := infile nextBit.
                "stdout << 'Curret node char ' << node char  << nl."
                readbit = 0 ifTrue: [
                    node isleaf ifTrue: [
                        node char asInteger = 256 ifTrue: [eof := true.
                        "stdout << 'EOF is: ' << eof << nl."].
                    ] ifFalse: [
                        node := node left.
                        node isleaf ifTrue: [
                            node char asInteger = 256
                                ifTrue: [eof := true.
                                "stdout << 'EOF is: ' << eof << nl."]
                                ifFalse: [
                                    "stdout << node char."
                                   
                                    outfile << node char.
                                  
                                ].
                            node := root.
                        ].

                    ].
                ] ifFalse: [
                    node isleaf ifTrue: [
                        node char asInteger = 256 ifTrue: [eof := true.
                        "stdout << 'EOF is: ' << eof << nl."].
                    ] ifFalse: [
                        node := node right.
                        node isleaf ifTrue: [
                            node char asInteger = 256
                                ifTrue: [eof := true.
                                "stdout << 'EOF is: ' << eof << nl."]
                                ifFalse: [
                                    "stdout << node char."
                                
                                   outfile << node char.
                                  
                            ].
                            node := root.
                        ].
                    ].
                ].
            ].
           " stdout << 'EXIT'  << nl."
           
    ].

].