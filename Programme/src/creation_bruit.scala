import com.tncy.top.image.ImageWrapper
object creation_bruit extends App {
  def binToInt(s: String): Int = {
    /*  Nom de la fonction : hexToInt
    entrée : Chaine de caractère, correspondant à un nombre en base 16
    retour : Entier, conversion du nombre héxadécimal de base.
 */
    s.toList.map("01".indexOf(_)).reduceLeft(_ * 2 + _)
  }

  def creation_image_bruit(): Unit = {
    var imageRefName: String = "paysage.png";
    var imageRef: ImageWrapper = new ImageWrapper(imageRefName)
    var imageref2D: Array[Array[Int]] = imageRef.getImage();
    var imagebruit2D: Array[Array[Int]] = imageRef.getImage();
    var chaine : String = "";
    val r = new scala.util.Random
    for (i <- 0 to imageRef.height - 1) {
      for (j <- 0 to imageRef.width - 1) {
          var chaine : String = "";
          for (k <- 0 to 31) {
            chaine = chaine + r.nextInt(2).toString
          }
          println(chaine)
          imagebruit2D(i)(j) = binToInt(chaine)
      }
    }
    println("Succès de la création du bruit ")
    var outputFile: String = "bruit.png"
    imageRef.saveImage(outputFile)
  }
  creation_image_bruit();

}
