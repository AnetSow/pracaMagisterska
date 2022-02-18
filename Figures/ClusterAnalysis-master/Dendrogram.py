from PyQt5.QtWidgets import QWidget
import scipy.cluster.hierarchy as sch
import scipy.cluster.vq as scv
import matplotlib.pyplot as plt
from matplotlib.backends.backend_qt5agg import NavigationToolbar2QT as NavigationToolbar
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas


class Dendrogram(QWidget):

    def __init__(self):
        super().__init__()
        self.figure = plt.figure()
        self.canvas = FigureCanvas(self.figure)
        self.canvas.setParent(self)
        self.toolbar = NavigationToolbar(self.canvas, self)

    def plot_kmean(self, X, k):
        self.figure.clf()
        ax = self.figure.add_subplot(111)
        # sch.dendrogram(sch.linkage(X, method='ward'), leaf_rotation=0, leaf_font_size=10)

        scv.kmeans2(X, k=k, iter=10, thresh=1e-05, minit='random', missing='warn', check_finite=True)
        print('Kmeans plotting...')
        ax.set_title('Cluster analysis with k-means method')
        ax.grid(True)
        self.canvas.draw_idle()
        print('Plotted!')

    def plot_agnes(self, X):
        self.figure.clf()
        ax = self.figure.add_subplot(111)
        sch.dendrogram(sch.linkage(X, method='ward'), leaf_rotation=0, leaf_font_size=10)
        print('Agnes plotting...')
        ax.set_title('Cluster analysis with agglomerative clustering method')
        ax.grid(True)
        self.canvas.draw_idle()
        print('Plotted!')

    def plot_slink(self, X):
        self.figure.clf()
        ax = self.figure.add_subplot(111)
        sch.dendrogram(sch.linkage(X, method='single'), leaf_rotation=0, leaf_font_size=10)
        print('Slink plotting...')
        ax.set_title('Cluster analysis with single linkage method')
        ax.grid(True)
        self.canvas.draw_idle()
        print('Plotted!')
