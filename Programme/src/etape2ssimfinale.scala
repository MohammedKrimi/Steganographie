import com.tncy.top.image.ImageWrapper
import scala.math.min
import SSIM.ssimPrincip


object etape2ssimfinale extends App {
  def binToInt(s: String): Int = {
    s.toList.map("01".indexOf(_)).reduceLeft(_ * 2 + _)
  }
  def remplissage(caract : Char, nbBit: Int, imageSrc2D : Array[Array[Int]], imageDest2D : Array[Array[Int]], i : Int ,j : Int): String = {
    //nbBit correspond ici au nombre de bit qu'on garde.
    var chaine: String = "";
    var biais: Int = 0;
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
    for(k <- 0 to 7) {
      if (k < nbBit) {
        chaine = chaine+(imageSrc2D(i)(j).toBinaryString)(k + biais);
      }
      else {
        chaine = chaine+(imageDest2D(i)(j).toBinaryString)(k + biais - nbBit);
      }
    }
    return chaine;
  }

  def encodssim() {
    println("Veuillez placer vos deux images dans le même dossier que le projet")
    var imageSrcName: String = readLine("Quel le nom de l'image innocente ? ") //Nom de la première image
    var imageDestName: String = readLine("Quel est le nom de l'image à cacher ? ") //Nom de la seconde image
    var outputFile: String = "outputImage.png"

    var imageSrc: ImageWrapper = new ImageWrapper(imageSrcName) //création de la nouvelle variable image source, du type ImageWrapper
    var imageDest: ImageWrapper = new ImageWrapper(imageDestName) //Idem pour l'image de destination.

    var imageSrc2D: Array[Array[Int]] = imageSrc.getImage(); //image2D = tableau composé d'un tableau d'entier pour chaque pixel, correspondants aux valeurs TRGB en hexadécimale de imageSrc
    var imageDest2D: Array[Array[Int]] = imageDest.getImage(); //Idem pour l'image de destination.

    var imageFinale2DInt: Array[Array[Int]] = imageSrc.getImage(); //Création d'un tableau de tableau de la même taille que l'image d'origine, remplis initialement de 0

    var imageFinale2DHex: Array[Array[String]] = Array.fill(imageSrc.height, imageSrc.width)("0")
    var chainefinale =""
    var nbBit1 =8
    var nbBitR = 8
    var nbBitV = 8
    var nbBitB = 8
    for (i <- 0 to min(imageSrc.height, imageDest.height) - 1) {
      for (j <- 0 to min(imageDest.width, imageDest.width) - 1) {
        chainefinale = remplissage('T', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('R', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('G', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('B', nbBit1, imageSrc2D, imageDest2D, i, j);
        imageFinale2DInt(i)(j) = binToInt(chainefinale);
        //print(binToInt(chainefinale))
        //À chaque pixel de l'image finale on stocke la chaîne modifié pour la transparence, le rouge, le vert et le bleu.
      }
    }
    imageSrc.saveImage(outputFile)

    var ssim = SSIM.ssimPrincip(imageSrcName, outputFile)
    if (ssim(0) >= 0.997) {
      while (ssim(0) >= 0.997 && nbBitR!=1) {
        //tant que les valeurs SSIM ne dépaasent pas les valeurs critiques de la bonne qualié de l'image innocentz on change un bit de plus
        nbBitR = nbBitR - 1
        for (i <- 0 to min(imageSrc.height, imageDest.height) - 1) {
          for (j <- 0 to min(imageDest.width, imageDest.width) - 1) {
            chainefinale = remplissage('T', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('R', nbBitR, imageSrc2D, imageDest2D, i, j) + remplissage('G', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('B', nbBit1, imageSrc2D, imageDest2D, i, j);
            imageFinale2DInt(i)(j) = binToInt(chainefinale);
            //À chaque pixel de l'image finale on stocke la chaîne modifié pour le rouge
          }
        }
        imageSrc.saveImage(outputFile)
        ssim = SSIM.ssimPrincip(imageSrcName, outputFile)
        //changement de la valeur qui nous permet de faire le test sur la nouvelle image
      }
      println("Valeur SSIM en rouge : " + ssim(0))
      println("le nombre des bits changés pour le rouge afin de garder une bonne qualité de l'image innocente est : " + (8-nbBitR))
    }
    if (ssim(1)>= 0.997) {
      while (ssim(1) >= 0.997 && nbBitV!=1) {
        //tant que les valeurs SSIM ne dépasent pas les valeurs critiques de la bonne qualié de l'image innocentz on change un bit de plus
        nbBitV = nbBitV - 1
        for (i <- 0 to min(imageSrc.height, imageDest.height) - 1) {
          for (j <- 0 to min(imageDest.width, imageDest.width) - 1) {
            chainefinale = remplissage('T', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('R', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('G', nbBitV, imageSrc2D, imageDest2D, i, j) + remplissage('B', nbBit1, imageSrc2D, imageDest2D, i, j);
            imageFinale2DInt(i)(j) = binToInt(chainefinale);
            //À chaque pixel de l'image finale on stocke la chaîne modifié pour le vert .
          }
        }
        imageSrc.saveImage(outputFile)
        ssim = SSIM.ssimPrincip(imageSrcName, outputFile)
        //changement de la valeur qui nous permet de faire le test sur la nouvelle image
      }
      println("Valeur SSIM en vert : " + ssim(1))
      println("le nombre des bits changés pour le vert afin de garder une bonne qualité de l'image innocente est : " + (8-nbBitV))
    }
    if (ssim(2)>= 0.997) {
      while (ssim(2) >= 0.997 && nbBitB!=1) {
        //tant que les valeurs SSIM ne dépaasent pas les valeurs critiques de la bonne qualié de l'image innocentz on change un bit de plus
        nbBitB = nbBitB - 1
        for (i <- 0 to min(imageSrc.height, imageDest.height) - 1) {
          for (j <- 0 to min(imageDest.width, imageDest.width) - 1) {
            chainefinale = remplissage('T', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('R', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('G', nbBit1, imageSrc2D, imageDest2D, i, j) + remplissage('B', nbBitB, imageSrc2D, imageDest2D, i, j);
            imageFinale2DInt(i)(j) = binToInt(chainefinale);
            //À chaque pixel de l'image finale on stocke la chaîne modifié pour le bleue.
          }
        }
        imageSrc.saveImage(outputFile)
        ssim = SSIM.ssimPrincip(imageSrcName, outputFile)
        //changement de la valeur qui nous permet de faire le test sur la nouvelle image
       
      }
      println("Valeur SSIM en bleue : " + ssim(2))
      println("le nombre des bits changés pour le bleue afin de garder une bonne qualité de l'image innocente est : " + (8-nbBitB))
      println("La clé de décryptage est : R"+nbBitR.toString+"G"+nbBitV.toString+"B"+nbBitB.toString)
    }
    else {
      println("On peut cacher l'image, sinon la qualié de l'image finale sera gravement affectée ")
    }
    imageSrc.saveImage(outputFile)
  }
}