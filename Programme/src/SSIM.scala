import com.tncy.top.image.ImageWrapper;
object SSIM extends App {
  def binToInt(s: String): Int = {
    /*  Nom de la fonction : hexToInt
        entrée : Chaine de caractère, correspondant à un nombre en base 16
        retour : Entier, conversion du nombre héxadécimal de base.
     */
    s.toList.map("01".indexOf(_)).reduceLeft(_ * 2 + _)
  }

  def pixel(couleur: Char, image: ImageWrapper, i: Int, j: Int): Int = {
    /*  Nom de la fonction : pixel
        entrée : couleur, image, i, j
        retour : valeur entière de la composante correspondante à la couleur 'couleur'.
    */
    var image2D: Array[Array[Int]] = image.getImage();
    var chaine : String = ""
    var biais : Int = 0
    if (couleur == 'T') {
      biais = 0
    }
    else if (couleur == 'R') {
      biais = 8
    }
    else if (couleur == 'G') {
      biais = 16
    }
    else {
      biais = 24
    }
    for (k <- 0 to 7) {
      chaine = chaine + (image2D(i)(j).toBinaryString)(k + biais);
    }
    return binToInt(chaine)
  }


  def calculMoyenne(image: ImageWrapper, couleur: Char): Float = {
    var somme: Float = 0;
    for (i <- 0 to image.height - 1) {
      for (j <- 0 to image.width - 1) {
        somme = somme + pixel (couleur, image, i, j)
      }
    }
    return somme / (image.height * image.width)
  }


  def calculVariance(image: ImageWrapper, couleur: Char, moyenne : Float): Float = {
    var somme: Float = 0;
    for (i <- 0 to image.height - 1) {
      for (j <- 0 to image.width - 1) {
        var stock:Float = (pixel (couleur, image, i, j) - moyenne);
        somme = somme + stock*stock
      }
    }
    return math.sqrt(somme/(image.height * image.width-1)).toFloat
  }


  def calculCov(moyennex : Float, moyenney : Float, image1: ImageWrapper, image2 : ImageWrapper, couleur: Char): Float = {
    var somme: Float = 0;
    for (i <- 0 to image1.height - 1) {
      for (j <- 0 to image1.width - 1) {
        somme = somme + (pixel (couleur, image1, i, j) - moyennex)*(pixel (couleur, image2, i, j) - moyenney)
      }
    }
    return math.abs(somme)/(image1.height * image1.width-1)
  }


  //******DEBUT DU PROGRAMME PRINCIPAL*******
  def ssimPrincip (image1Name : String, image2Name : String) : Array[Float] ={
    var couleur: Array[Char] = new Array[Char](3);
    couleur(0) = 'R'
    couleur(1) = 'G'
    couleur(2) = 'B'
    val image1: ImageWrapper = new ImageWrapper(image1Name)
    val image2: ImageWrapper = new ImageWrapper(image2Name)
    var moyennex: Float = 0;
    var variancex: Float = 0;
    var moyenney: Float = 0;
    var variancey: Float = 0;
    var covxy: Float = 0;
    var ssimcouleur: Float = 0;
    var ssim: Array[Float] =  Array.fill(3)(0);
    var compte: Int = 0;
    var C1:Float = ((255*0.01)*(255*0.01)).toFloat;
    var C2:Float = ((255*0.01)*(255*0.01)).toFloat;
    //On parcours chaque couleur et on ajoute le SSIM correspondant à une couleur donnée
    for (a <- couleur) {
      moyennex = calculMoyenne(image1, a)
      variancex = calculVariance(image1, a, moyennex)
      moyenney = calculMoyenne(image2, a)
      variancey = calculVariance(image2, a, moyenney)
      covxy = calculCov(moyennex, moyenney, image1, image2, a)
      ssimcouleur = ((2*moyennex*moyenney+C1)*(2*covxy+C2))/((moyennex*moyennex + moyenney*moyenney+C1)*(variancex*variancex + variancey*variancey+C2));
      println()
      ssim(compte) = ssimcouleur;
      compte += 1
    }
    return ssim
  }
  var ssim1 = ssimPrincip("image1.png","image2.png")
  println("Valeur SSIM en rouge : " +ssim1(0))
  println("Valeur SSIM en vert : " +ssim1(1))
  println("Valeur SSIM en bleu : " +ssim1(2))
}
