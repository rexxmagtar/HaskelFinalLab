# HaskelFinalLab

Дерево кодирования Хаффмана представлено типом данных Node. Узел — это либо лист, у которого есть символ и вес, либо внутренний узел, у которого есть левый узел, правый узел, список символов в дереве и совокупный вес.

Вы можете построить дерево кодирования Хаффмана, передав список конечных узлов, отсортированных по весу каждого листа, в makeTree следующим образом.

пусть дерево = makeTree [(Node 'A' 1), (Node 'B' 1), (Node 'C' 2)]
makeTree работает, беря два узла с наименьшими весами из списка и объединяя их в новый узел, а затем вставляя новый узел в список в правильном месте.

encode работает, беря символ из текстовой строки и ища этот символ в дереве. Когда encode перемещается вниз по дереву к листьям, он добавляет бит к результирующей битовой строке. Когда кодирование достигает листа, кодирование возвращается к корню дерева кодирования Хаффмана и берет следующий символ из текстовой строки. Когда encode выходит за пределы символов, он возвращается.

декодирование работает аналогичным образом. decode берет бит из строки битов и перемещается в левое поддерево, если бит равен 0, и перемещается в правое поддерево в противном случае. Когда декодирование достигает листа, он вставляет символ листа в результирующую строку, возвращается к корню дерева кодирования Хаффмана и берет следующий бит из битовой строки.
