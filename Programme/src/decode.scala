
import com.tncy.top.image.ImageWrapper


object decode {
  def decod() {
    def binToInt(s: String): Int = {
      /*  Nom de la fonction : hexToInt
        entrée : Chaine de caractère, correspondant à un nombre en base 16
        retour : Entier, conversion du nombre héxadécimal de base.
     */
      s.toList.map("01".indexOf(_)).reduceLeft(_ * 2 + _)
    }
    println("Veuillez placer l'image à décoder dans le dossier du projet")
    var fileName1: String = readLine("Quelle est l'image à décoder ? ")
    var cle: String = readLine("Quel est la clé de décryptage ? ")
    var nbBitR = cle(1).toString.toInt
    var nbBitV = cle(3).toString.toInt
    var nbBitB = cle(5).toString.toInt

    var yes: ImageWrapper = new ImageWrapper(fileName1);

    var image2D: Array[Array[Int]] = yes.getImage();

    def decodage(caract: Char, nbBit: Int, imageCod: Array[Array[Int]], i: Int, j: Int): List[String] = {
      //nbBit correspond ici au nombre de bit qu'on garde.
      var chaine1: String = "";
      var chaine2: String = ""
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
          chaine1 = chaine1 + (imageCod(i)(j).toBinaryString) (k + biais);
        }
        else {
          chaine1 = chaine1 + "0";
          chaine2 = chaine2 + (imageCod(i)(j).toBinaryString) (k + biais);
        }
      }
      for (k<- 0 to nbBit-1) {
        chaine2 = chaine2 + "0"
      }
      return List(chaine1 , chaine2);
    }


    var nbBit = 1
    var image1Bin: Array[Array[String]] = Array.fill(yes.height, yes.width)("0")
    var image2Bin: Array[Array[String]] = Array.fill(yes.height, yes.width)("0")

    for (i <- 0 to yes.height - 1) {
      for (j <- 0 to yes.width - 1) {
        image1Bin(i)(j) = decodage('T', nbBit, image2D, i, j).head + decodage('R', nbBitR, image2D, i, j).head + decodage('G', nbBitV, image2D, i, j).head + decodage('B', nbBitB, image2D, i, j).head
        image2Bin(i)(j) = decodage('T', nbBit, image2D, i, j).tail.head + decodage('R', nbBitR, image2D, i, j).tail.head + decodage('G', nbBitV, image2D, i, j).tail.head + decodage('B', nbBitB, image2D, i, j).tail.head
      }
    }

    for (i <- 0 to yes.height - 1) {
      for (j <- 0 to yes.width - 1) {
        image2D(i)(j) = binToInt(image1Bin(i)(j))
      }
    }
    var outputFile: String = "imagecache1.png"
    yes.saveImage(outputFile)

    for (i <- 0 to yes.height - 1) {
      for (j <- 0 to yes.width - 1) {
        image2D(i)(j) = binToInt(image2Bin(i)(j))
      }
    }
    outputFile = "imagecache2.png"
    yes.saveImage(outputFile)
  }
}
