import patmat.Huffman._

object Main extends App {
    var list = List('A', 'B', 'C', 'A', 'C', 'A', 'D')
    var tree = createCodeTree(list)
    var text = decode( tree,  List(0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0))
    println( list )
    println( encode( tree )( list ) )
    println( decode( tree, encode( tree )( list ) ) )
    println( quickEncode( tree )( decode( tree, encode( tree )( list ) ) ) )
    println( decode( tree, encode( tree )( decode( tree, encode( tree )( list ) ) ) ) )
    println( decodedSecret )
    println( convert(tree) )
}
