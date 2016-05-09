package compy

/**
 * A class which represents an entry in a static table.
 * This includes static variables, jumps, and multi-use memory
 */
class StaticEntry(val id: Char, private var address: Integer = 0) {

  def setAddress(arg: Integer): Unit = {
    if (arg < 0 || arg > 0xff)
      throw new Exception("Addresses must be on the interval [0x0, 0x100)")
    address = arg
  }

  def getAddress(): Int = address

  def getAddressString(): String = "%02X".format(address)

  override def toString(): String = "<" + id + ", " + getAddressString + ">"
  
}
