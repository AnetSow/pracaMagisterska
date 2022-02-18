from PyQt5.QtWidgets import QWidget
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt5agg import NavigationToolbar2QT as NavigationToolbar
import matplotlib.pyplot as plt

class Plot(QWidget):

    def __init__(self):
        super().__init__()
        self.figure = plt.figure()
        self.canvas = FigureCanvas(self.figure)
        self.canvas.setParent(self)
        self.toolbar = NavigationToolbar(self.canvas, self)

    def plot(self, name):
        self.figure.clf()
        ax = self.figure.add_subplot(111)
        ax.plot(name, 'o')

        ax.set_title('Oryginal dataset')
        ax.grid(True)
        self.canvas.draw_idle()
        print('Plotted')
