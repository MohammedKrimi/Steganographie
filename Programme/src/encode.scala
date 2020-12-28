import com.tncy.top.image.ImageWrapper
import math.min

object encode {
  def encod(imageSrcName:String, imageDestName:String, nbBit:Int)  {

    def binToInt(s: String): Int = {
      /*  Nom de la fonction : hexToInt
        entrée : Chaine de caractère, correspondant à un nombre en base 16
        retour : Entier, conversion du nombre héxadécimal de base.
     */
      s.toList.map("01".indexOf(_)).reduceLeft(_ * 2 + _)
    }

    def remplissage(caract: Char, nbBit: Int, imageSrc2D: Array[Array[Int]], imageDest2D: Array[Array[Int]], i: Int, j: Int): String = {
      //nbBit correspond ici au nombre de bit qu'on garde.
      var chaine: String = "";
      var biais: Int = 0
      //On définit un biais en fonction de la partie interessante dans le code binaire de chaque pixel (ex: de 0 à 4 exclu, il s'agit du code pour la transparence)
      if (caract == 'T') {
        biais = 0
      }
      else if (caract == 'R') {
        biais = 8
      }
      else if (caract == 'G') {
        biais = 16
      }
      else if (caract == 'B') {
        biais = 24
      }
      for (k <- 0 to 7) {
        if (k < nbBit) {
          chaine = chaine + (imageSrc2D(i)(j).toBinaryString) (k + biais)
        }
        else {
          chaine = chaine + (imageDest2D(i)(j).toBinaryString) (k + biais - nbBit);
        }
      }
      return chaine;
    }
    println("Veuillez placer vos deux images dans le même dossier que le projet")
    var imageSrc: ImageWrapper = new ImageWrapper(imageSrcName) //création de la nouvelle variable image source, du type ImageWrapper
    var imageDest: ImageWrapper = new ImageWrapper(imageDestName) //Idem pour l'image de destination.

    var imageSrc2D: Array[Array[Int]] = imageSrc.getImage(); //image2D = tableau composé d'un tableau d'entier pour chaque pixel, correspondants aux valeurs TRGB en hexadécimale de imageSrc
    var imageDest2D: Array[Array[Int]] = imageDest.getImage(); //Idem pour l'image de destination.

    var imageFinale2DInt: Array[Array[Int]] = imageSrc.getImage(); //Création d'un tableau de tableau de la même taille que l'image d'origine, remplis initialement de 0

    for (i <- 0 to min(imageSrc.height,imageDest.height) - 1) {
      for (j <- 0 to min(imageDest.width,imageDest.width) - 1) {
        var chainefinale: String = remplissage('T', nbBit, imageSrc2D, imageDest2D, i, j) + remplissage('R', nbBit, imageSrc2D, imageDest2D, i, j) + remplissage('G', nbBit, imageSrc2D, imageDest2D, i, j) + remplissage('B', nbBit, imageSrc2D, imageDest2D, i, j)
        //println(remplissage('B', nbBit, imageSrc2D, imageDest2D, i, j))
        //println(chainefinale)
        imageFinale2DInt(i)(j) = binToInt(chainefinale)
        //print(binToInt(chainefinale))
        //À chaque pixel de l'image finale on stocke la chaîne modifié pour la transparence, le rouge, le vert et le bleu.
      }
    }
    println("Les deux images ont fusionné dans l'image appelé outputImage.png")
    var outputFile: String = "outputImage.png"
    imageSrc.saveImage(outputFile)

  }
}
