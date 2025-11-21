data ArbolB a = Void | Node (ArbolB a) a (ArbolB a) 
                deriving (Show, Eq) 

podar :: ArbolB a -> Int -> ArbolB a
podar _ 0 = Void
podar (Node t1 x t2) n = Node (podar t1 (n-1)) x (podar t2 (n-1))

elementosProfundidad :: ArbolB a -> Int -> [a]
elementosProfundidad Void _ = []
elementosProfundidad (Node t1 x t2) 0 = [x]
elementosProfundidad (Node t1 _ t2) n = elementosProfundidad t1 (n-1) ++ elementosProfundidad t2 (n-1)

arbolCompleto6 :: ArbolB Int
arbolCompleto6 =
  Node
    (Node
      (Node
        (Node
          (Node
            (Node
              (Node Void 1 Void)
              2
              (Node Void 3 Void))
            4
            (Node
              (Node Void 5 Void)
              6
              (Node Void 7 Void)))
          8
          (Node
            (Node
              (Node Void 9 Void)
              10
              (Node Void 11 Void))
            12
            (Node
              (Node Void 13 Void)
              14
              (Node Void 15 Void))))
        16
        (Node
          (Node
            (Node
              (Node Void 17 Void)
              18
              (Node Void 19 Void))
            20
            (Node
              (Node Void 21 Void)
              22
              (Node Void 23 Void)))
          24
          (Node
            (Node
              (Node Void 25 Void)
              26
              (Node Void 27 Void))
            28
            (Node
              (Node Void 29 Void)
              30
              (Node Void 31 Void)))))
      32
      (Node
        (Node
          (Node
            (Node
              (Node Void 33 Void)
              34
              (Node Void 35 Void))
            36
            (Node
              (Node Void 37 Void)
              38
              (Node Void 39 Void)))
          40
          (Node
            (Node
              (Node Void 41 Void)
              42
              (Node Void 43 Void))
            44
            (Node
              (Node Void 45 Void)
              46
              (Node Void 47 Void))))
        48
        (Node
          (Node
            (Node
              (Node Void 49 Void)
              50
              (Node Void 51 Void))
            52
            (Node
              (Node Void 53 Void)
              54
              (Node Void 55 Void)))
          56
          (Node
            (Node
              (Node Void 57 Void)
              58
              (Node Void 59 Void))
            60
            (Node
              (Node Void 61 Void)
              62
              (Node Void 63 Void))))))
    64
    (Node
      (Node
        (Node
          (Node
            (Node
              (Node Void 65 Void)
              66
              (Node Void 67 Void))
            68
            (Node
              (Node Void 69 Void)
              70
              (Node Void 71 Void)))
          72
          (Node
            (Node
              (Node Void 73 Void)
              74
              (Node Void 75 Void))
            76
            (Node
              (Node Void 77 Void)
              78
              (Node Void 79 Void))))
        80
        (Node
          (Node
            (Node
              (Node Void 81 Void)
              82
              (Node Void 83 Void))
            84
            (Node
              (Node Void 85 Void)
              86
              (Node Void 87 Void)))
          88
          (Node
            (Node
              (Node Void 89 Void)
              90
              (Node Void 91 Void))
            92
            (Node
              (Node Void 93 Void)
              94
              (Node Void 95 Void)))))
      96
      (Node
        (Node
          (Node
            (Node
              (Node Void 97 Void)
              98
              (Node Void 99 Void))
            100
            (Node
              (Node Void 101 Void)
              102
              (Node Void 103 Void)))
          104
          (Node
            (Node
              (Node Void 105 Void)
              106
              (Node Void 107 Void))
            108
            (Node
              (Node Void 109 Void)
              110
              (Node Void 111 Void))))
        112
        (Node
          (Node
            (Node
              (Node Void 113 Void)
              114
              (Node Void 115 Void))
            116
            (Node
              (Node Void 117 Void)
              118
              (Node Void 119 Void)))
          120
          (Node
            (Node
              (Node Void 121 Void)
              122
              (Node Void 123 Void))
            124
            (Node
              (Node Void 125 Void)
              126
              (Node Void 127 Void))))))
