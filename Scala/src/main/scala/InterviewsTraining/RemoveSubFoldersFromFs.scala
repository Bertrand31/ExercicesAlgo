// https://leetcode.com/problems/remove-sub-folders-from-the-filesystem/

object RemoveSubFoldersFromFs:

  final case class Trie(isEnd: Boolean, subDirs: Map[String, Trie] = Map.empty)

  private def applyPath(trie: Trie, path: List[String]): Trie =
    path match
      case Nil => trie.copy(isEnd = true)
      case head :: Nil =>
        trie.subDirs.get(head) match
          case Some(subTrie) if subTrie.isEnd => trie
          case Some(subTrie) =>
            trie.copy(
              subDirs = trie.subDirs.updated(head, subTrie.copy(true, Map.empty)),
            )
          case None => trie.copy(
            subDirs = trie.subDirs.updated(head, Trie(true))
          )
      case head :: next =>
        trie.subDirs.get(head) match
          case Some(subTrie) if subTrie.isEnd => trie
          case Some(subTrie) => trie.copy(
            subDirs = trie.subDirs.updated(head, applyPath(subTrie, next))
          )
          case None => trie.copy(
            subDirs = trie.subDirs.updated(head, applyPath(Trie(false), next))
          )

  private def getAllPathsFromTrie(trie: Trie): List[String] =
    if trie.isEnd then List("")
    else
      trie.subDirs.flatMap({
        case (subStr, subTrie) => getAllPathsFromTrie(subTrie).map(subPaths => "/" ++ subStr ++ subPaths)
      }).toList

  def removeSubfolders(folder: Array[String]): List[String] =
    val paths = folder.map(_.split('/').tail.toList).toList
    val trie = paths.foldLeft(Trie(false))(applyPath)
    getAllPathsFromTrie(trie)

object RemoveSubFoldersFromFsApp extends App:

  {
    val paths = Array("/a", "/a/b", "/c/d", "/c/d/e", "/c/fe")
    val expected = List("/a", "/c/d", "/c/fe")
    val res = RemoveSubFoldersFromFs.removeSubfolders(paths)
    assert(res == expected)
  }
  {
    val paths = Array("/ah/al/me", "/ah/al")
    val expected = List("/ah/al")
    val res = RemoveSubFoldersFromFs.removeSubfolders(paths)
    assert(res == expected)
  }
