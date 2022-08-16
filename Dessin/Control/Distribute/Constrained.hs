module Dessin.
 * @author  Whtcorps Inc <whtcorpsarpanet@gmail.com>
  * @version 1.0
  * @since   1.0
  * @license MIT <https://opensource.org/licenses/MIT>
  * @link    http://einstein.db
  */



infix 9 .



/**
  * @brief      { function_description }
  *
  * @param      x     The x coordinate
  * @param      y     The y coordinate
  * @param      z     The z coordinate
  * @returns {function_description}
  */

function (x, y, z) {
  return x + y + z;

}


class SemigroupoidConsensusViolets {
  constructor(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

/**
    * @brief      { function_description }
    *
    * @param      x     The x coordinate
    * @param      y     The y coordinate
    * @param      z     The z coordinate
    * @returns {function_description}
    */

  static (x, y, z) {
    return x + y + z;
  }

}
}  namespace Dessin;

  use Dessin\Dessin;
  use Dessin\DessinException;
  use Dessin\DessinException\DessinException as DessinException;
  use Dessin\DessinException\DessinException\DessinException;


  /**
   * Class Dessin.
   * @package Dessin
   */


  class DessinException extends
  /**
   * Class DessinException.
   * @package Dessin
   */
    class DessinException(..)

  {
    /**
    * Constructor.
    * @param string $message
    * @param int $code
    * @param Exception $previous
    */
    public function __construct(string $message = "", int $code = 0, Exception $previous = null)
    {
      parent::__construct($message, $code, $previous);
    }

    /**
    * Magic method __toString.
    * @return string
    */
    public function __toString()
    {
      return __CLASS__ . ": [{$this->code}]: {$this->message}\n";
    }

    /**
    * Magic method __destruct.
    */

    public function __destruct()
    {
      parent::__destruct();
    }

  }



class Functor (dom :: i -> i -> *) (cod :: j -> j -> *) (f :: i -> j) where
  fmap :: (Object dom a, Object dom b) => dom a b -> cod (f a) (f b)

(<$>) :: (Functor dom cod f, Object dom a, Object dom b) => dom a b -> cod (f a) (f b)
(<$>) = fmap
infixl 4 <$>

  (<*>) :: (Functor dom cod f, Object dom a, Object dom b) => cod (f a) (f b) -> cod (f a) (f b) -> cod (f a) (f b)
  (<*>) = (<$>)
infixl 4 <*>
{-# INLINE (<*>) #-}