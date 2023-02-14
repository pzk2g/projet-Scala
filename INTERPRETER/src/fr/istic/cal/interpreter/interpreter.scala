package fr.istic.cal.interpreter

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]
  
  

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  def lookUp(v: Variable, mem: Memory): Value = {
     mem match {
      case Nil                     => NlValue
      case (vmem, valeur) :: list  => if (v == vmem)  valeur  else lookUp(v,list)
    }
  }
  
  

  /**
   * @param v : une variable
   * @param d : une valeur
   * @param mem : une mémoire
   * @return la mémoire, augmentée de l'assignation [v->d] si v n'était pas présente dans la mémoire,
   * modifiée pour prendre en compte la nouvelle valeur de v sinon
   */
  def assign(v: Variable, d: Value, mem: Memory): Memory = {
     mem match {
      case Nil                    => (v,d) :: Nil
      case (vmem, valeur) :: list => if (vmem == v) (v,d) :: list   else (vmem, valeur) :: assign(v,d,list)
    }
  }

  
  
  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match {
      case Nl               => NlValue
      case Cst(s)           => CstValue(s)
      case VarExp(s)        => lookUp(Var(s), mem)
      case Cons(exp1, exp2) => ConsValue(interpreterExpr(exp1, mem) , interpreterExpr(exp2, mem))
      case Hd(exp)          => interpreterExpr(exp,mem) match {
                                  case ConsValue(a, b) => a
                                  case _               => NlValue
                                  }
      case Tl(exp)          => interpreterExpr(exp,mem) match {
                                  case ConsValue(a, b) => b
                                  case _               => NlValue
                                  }
      case Lt(exp)          => interpreterExpr(exp,mem) match {
                                  case ConsValue(_, CstValue(a)) => CstValue(a)
                                  case ConsValue(a, b) => interpreterExpr(Lt(valueToExpression(b)),mem)
                                  case _               => NlValue
                                  }
      case Eq(exp1 ,exp2)   => 
                                  if(interpreterExpr(exp1,mem) == interpreterExpr(exp2,mem)) ConsValue(NlValue, NlValue)
                                  else NlValue
      }
  }

  
  
  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant une expression de cette valeur
   */
  def valueToExpression(value: Value): Expression = {
    value match {
      case NlValue           => Nl
      case CstValue(n)       => Cst(n)
      case ConsValue(v1, v2) => Cons(valueToExpression(v1), valueToExpression(v2))
    }
  }
  
  

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  def interpreterCommand(command: Command, memory: Memory): Memory = {
    command match {
      case Nop                         => memory
      case Set(v, exp)                 => assign(v, interpreterExpr(exp, memory), memory)
      case While(condExp, listC)       => interpreterExpr(condExp, memory) match {
                                            case NlValue => memory
                                            case _ => interpreterCommand(While(condExp, listC), interpreterCommands(listC, memory))
                                          }
      case For(exp, listC)             =>
                                        val v : Value = interpreterExpr(exp, memory)
                                          v match {
                                            case NlValue => memory
                                            case _ => interpreterCommand(For(Tl(valueToExpression(v)), listC), interpreterCommands(listC, memory))
                                          }
      case If(exp, listThen, listElse) => interpreterExpr(exp, memory) match {
                                            case NlValue => interpreterCommands(listElse, memory)
                                            case _ => interpreterCommands(listThen, memory)
                                          }
    }
  }

  
  
  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    commands match {
      case Nil        => throw ExceptionListeVide 
      case c :: Nil   => interpreterCommand(c, memory)
      case c :: list  => interpreterCommands(list, interpreterCommand(c, memory))
    }
  }
  
  

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste non vide de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
    if(vars.length != vals.length) throw ExceptionListesDeLongueursDifferentes
    else {
      (vars, vals) match {
        case (variable :: Nil, valeur :: Nil)     => (variable, valeur) :: Nil
        case (variable :: list1, valeur :: list2) => (variable, valeur) :: interpreterMemorySet(list1, list2)
        case _                                    => throw ExceptionListeVide 
                                                     // Seul le cas (Nil, Nil) est attendu ici, le joker est utiliser pour eviter un warning

      }
    }
  }

  
  
  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
    vars match {
      case Nil              => throw ExceptionListeVide
      case variable :: Nil  => lookUp(variable, memory) :: Nil
      case variable :: list => lookUp(variable, memory) :: interpreterMemoryGet(list, memory)
    }
  }
  
  

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  def interpreter(program: Program, vals: List[Value]): List[Value] = {
    program match {
      case Progr(in, body, out) => interpreterMemoryGet(out, interpreterCommands(body, interpreterMemorySet(in,vals)))
    }
  }
  
  
  
  /**
   * 
   * Durant ce projet, les éléments que nous avons trouvés les plus difficiles à réaliser sont :
   *  - la récursivité mutuelle des fonctions interpreterCommand et interpreterCommands
   *  - la sémantique de la boucle for (avec la variable v et l'expression e)
   * 
   * En ajoutant l'expression Lt, il a été aussi assez compliqué de visualiser l'écriture de la sémantique 
   * de l'expression avec le type Value et d'écrire les résultats attendus des tests associés
   * 
   */
  
  
  
}