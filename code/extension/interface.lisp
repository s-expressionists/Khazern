(in-package #:khazern-extension)

(defmacro define-iteration-path (client-class)
  `(progn
     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :element)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-elements :var var)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :elements)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-elements :var var)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :byte)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-bytes :var var)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :bytes)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-bytes :var var)))
     
     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :character)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-characters :var var)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :characters)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-characters :var var)))
     
     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :object)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-objects :var var)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :objects)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-objects :var var)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :line)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-lines :var var)))

     (defmethod khazern:make-iteration-path
         ((client ,client-class) (name (eql :lines)) var
          &optional (inclusive-form nil inclusive-form-p))
       (declare (ignore inclusive-form))
       (if inclusive-form-p
           (call-next-method)
           (make-instance 'for-as-lines :var var)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :loop/khazern-extension *features*))
