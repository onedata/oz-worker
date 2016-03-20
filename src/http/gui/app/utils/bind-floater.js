/**
  Makes an element fixed positioned, with coordinates (left, top) of its parent.

  @param element A jQuery element which will float (onlu single!)
  @param parent (optional) A jQuery element which will act as element position parent.
          If not provided - will use a element.parent().
  @returns {function} Function which re-computes new fixed position of an element.
              It should be bind to eg. parent mouseover.
 */
export default function bindFloater(element, parent) {
  parent = parent || element.parent();
  element.addClass('.floater');
  let changePos = function() {
    let offset = parent.offset();
    let left = `${parseInt(offset.left) + parent.width()}px`;
    let top = offset.top;
    element.css({left: left, top: top});
  };

  changePos();
  return changePos;
}
