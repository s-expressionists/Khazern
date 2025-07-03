(in-package #:khazern-extension)

(defmacro define-iteration-path (client-class)
  `(progn
     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :element))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-elements)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :elements))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-elements)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :byte))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-bytes)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :bytes))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-bytes)))
     
     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :character))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-characters)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :characters))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-characters)))
     
     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :object))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-objects)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :objects))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-objects)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :line))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-lines)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :lines))
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-lines)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :loop/khazern-extension *features*)
