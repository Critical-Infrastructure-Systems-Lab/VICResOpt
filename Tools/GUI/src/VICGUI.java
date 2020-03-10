import java.awt.EventQueue;

import javax.swing.JFrame;
import javax.swing.JTabbedPane;
import java.awt.BorderLayout;
import javax.swing.JMenuBar;
import java.awt.Label;
import javax.swing.JTable;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import javax.swing.JLabel;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.RowSpec;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import java.awt.GridLayout;
import javax.swing.SwingConstants;
import javax.swing.JList;


public class VICGUI {

	private JFrame frmVicgui;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					VICGUI window = new VICGUI();
					window.frmVicgui.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public VICGUI() {
		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frmVicgui = new JFrame();
		frmVicgui.setTitle("VICGUI");
		frmVicgui.setBounds(100, 100, 778, 583);
		frmVicgui.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		JMenuBar menuBar = new JMenuBar();
		frmVicgui.setJMenuBar(menuBar);
		
		JMenu mnFile = new JMenu("File");
		menuBar.add(mnFile);
		
		JMenuItem mntmNew = new JMenuItem("New");
		mnFile.add(mntmNew);
		
		JMenuItem mntmOpen = new JMenuItem("Open");
		mnFile.add(mntmOpen);
		
		JMenuItem mntmClose = new JMenuItem("Close");
		mnFile.add(mntmClose);
		
		JMenuItem mntmExit = new JMenuItem("Exit");
		mnFile.add(mntmExit);
		frmVicgui.getContentPane().setLayout(new FormLayout(new ColumnSpec[] {
				ColumnSpec.decode("762px"),},
			new RowSpec[] {
				RowSpec.decode("523px"),}));
		
		JTabbedPane tabbedPane = new JTabbedPane(JTabbedPane.TOP);
		frmVicgui.getContentPane().add(tabbedPane, "1, 1, fill, fill");
		
		JPanel panel = new JPanel();
		tabbedPane.addTab("Pre-processing", null, panel, null);
		panel.setLayout(new GridLayout(10, 10, 2, 2));
		
		JPanel panel_1 = new JPanel();
		tabbedPane.addTab("Model calibration", null, panel_1, null);
		
		JPanel panel_2 = new JPanel();
		tabbedPane.addTab("Simulation", null, panel_2, null);
		
		JPanel panel_3 = new JPanel();
		tabbedPane.addTab("Sensitivy analysis", null, panel_3, null);
		
		JPanel panel_4 = new JPanel();
		tabbedPane.addTab("Reservoir optimization", null, panel_4, null);
		
		JPanel panel_5 = new JPanel();
		tabbedPane.addTab("Visualization", null, panel_5, null);
		
		
	}

}
