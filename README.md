## L-systems

My goal is to implement all features of the L-system described in 'Algorithmic
Beauty of Plants' (ABOP). The input will be from files using the grammar
described in ABOP. The result will be either an SVG (for 2D models) or
something else for 3D (I haven't settled on a format yet, but should be
something that can be read by Blender).

## Examples

### DOL-systems

The current user interface for entering these patterns is really clunky. I am
currently writing a parser to read in a file that describes the system.

![n=2 a=90 w=F-F-F-F F->FF+F+F-FFF-FFF-FFF-F+F+FF](images/2_90_F-F-F-F_F+FF-FF-F-F+F+FF-F-F+F+FF+FF-F.png)

![n=3 a=90 w=F-F-F-F F->FF-F--F-F](images/3_90_F-F-F-F_FF-F--F-F.png)

![n=3 a=90 w=F-F-F-F F->F+F+F-F-F+F-F-F+F-F-F+F+F](images/3_90_F-F-F-F_F+F+F-F-F+F-F-F+F-F-F+F+F.png)

![n=3 a=90 w=F-F-F-F F->F-F+F-F+F](images/3_90_F-F-F-F_F-F+F-F+F.png)

![n=3 a=90 w=F-F-F-F F->F-F-F+F+F-F-F+F+F](images/3_90_F-F-F-F_F-F-F+F+F-F-F+F+F.png)

![n=4 a=90 w=F-F-F-F F->FF-F+F+F-F](images/4_90_F-F-F-F_FF-F+F+F-F.png)

![n=4 a=90 w=F-F-F-F F->FF-F-F+F+F-F+F+F-F+F+F-F-FF](images/4_90_F-F-F-F_FF-F-F+F+F-F+F+F-F+F+F-F-FF.png)

![n=5 a=90 w=F F->F+F-F-F+F](images/5_90_F_F+F-F-F+F.png)
