import com.tncy.top.image.ImageWrapper
import scala.math.{log10, pow};
import decode.decod
import encode.encod
import creation_bruit.creation_image_bruit
import SSIM.ssimPrincip

object placerest extends App{
  def max(tableau:Array[Float]) : Float = {
    var maximum:Float = tableau(0)
    for (k <- 1 to tableau.length - 1){
      if (tableau(k) > maximum) {
        maximum = tableau(k)
      }
    }
    return maximum
  }
  var imageName: String = "image2.png";
  def placerestante(imageName: String)= {
    var image: ImageWrapper = new ImageWrapper(imageName) //création de la nouvelle variable image source, du type ImageWrapper
    var image2D: Array[Array[Int]] = image.getImage(); //image2D = tableau composé d'un tableau d'entier pour chaque pixel, correspondants aux valeurs TRGB en hexadécimale de imageSrc
    creation_image_bruit()
    //On fait varier le nombre de bit qu'on supprime de l'image initiale (on remplace par une information aléatoire)
    //On calcule le SSIM  pour chaque couleurs entre l'image initiale et l'image avec l'information remplacée
    //Si le SSIM dépasse la valeur seuil préalablement fixé, on conserve la dernière valeur
    //On en déduit une quantité d'information maximale stockable dans l'image.
    var nbBit: Int = 8
    var valeurSeuilSSIM: Float = 0.999.toFloat
    var ssim: Float = 1;
    while (nbBit > 0 && ssim > valeurSeuilSSIM) {
      encod(imageName, "bruit.png", nbBit); //On cache le bruit dans l'image avec un nombre k de bits modifiés.
      var tabSSIM: Array[Float] = ssimPrincip(imageName, "outputImage.png");
      ssim = max(tabSSIM)
      nbBit = nbBit - 1;
      println("Valeurs SSIM obtenue en supprimant " + (8 - nbBit) + " bits sur chaque pixel : " + ssim )
    }
    var nbPixel: Int = image.height * image.width;
    var qttInfoStock: Int = nbPixel * 2 ^ (8 - nbBit);
    println("L'image reste dans une qualité acceptable si on modifie aléatoirement " + (8 - nbBit) + " bits sur chaque pixel. ")
    println("Ainsi, on peut stocker " + qttInfoStock + " bits de données")
    println("Ceci correspond par exemple à une image de même taille, avec " +(8 - nbBit)+" bits d'information par pixels.")
  }
}
