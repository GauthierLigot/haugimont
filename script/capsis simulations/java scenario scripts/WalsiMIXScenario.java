package gbx.walsi.myscripts.haugimont;

import java.util.Collection;

import capsis.app.C4Script;
import capsis.kernel.Engine;
import capsis.kernel.Step;
import capsis.lib.multicriteriathinner2.MultiCriteriaThinner2;
import capsis.lib.multicriteriathinner2.MultiCriteriaThinner2ParameterLoader;
import gbx.walsi.extension.ioformat.WalsiByClassExport;
import gbx.walsi.model.WalsiEvolutionParameters;
import gbx.walsi.model.WalsiInitialParameters;
import gbx.walsi.model.WalsiMethodProvider;
import gbx.walsi.model.WalsiModel;
import gbx.walsi.model.WalsiScene;
import jeeb.lib.util.PathManager;

/**
 * A Walsi script used to simulate a reference scenario of an unevenaged oak stand.
 * 
 * <pre>
 * # To launch the script from a terminal (windows): 
 * capsis -p script gbx.walsi.myscripts.SimpleScript
 * 
 * # To launch the script from a terminal (Linux / Mac): 
 * sh capsis.sh -p script gbx.walsi.myscripts.SimpleScript
 * </pre>
 * 
 * @author Gauthier Ligot - July 2025
 */

public class WalsiMIXScenario {

	public static void main(String[] args) throws Exception {

		C4Script script = new C4Script("gbx.walsi");

		String inputDirectory = "C:\\OneDrive\\OneDrive - Universite de Liege\\PROJECT\\HAUGIMONT\\4_ANALYSE ECONOMIQUE"
				+ "\\8_Simulations\\4_unevenaged\\";

		String[] scenarios = {"1","2","3"};
		String inventory = inputDirectory + "haugimont1992.inv";
		String species = PathManager.getDir ("data") + "/gbx/walsi/Walsi.species"; //inputDirectory + "Walsi.species";
		String climate = inputDirectory + "1900_2500_constantClimate_Haugimont.txt";
		String thinning = inputDirectory + "thinning parameters";
		String thinning2 = inputDirectory + "thinning parameters frene";

		int cuttingCycle = 8; // years

		for(String scenario : scenarios) {

			WalsiInitialParameters ip = new WalsiInitialParameters(inventory, species, climate);

			ip.growthModel = "SIMREG_COEF";
			ip.growthModelSimregCoef = 1.6; // gl 27-11-2025 the productivity was too low
			
			if(scenario == "2") {
				ip.growthModel = "SIMREG_COEF";
				ip.growthModelSimregCoef = 0.8 * 1.6;
			}

			// initialization
			script.init(ip);
			WalsiModel model = (WalsiModel) script.getModel(); // fc-19.6.2025 Reproducibility
			Step step = script.getLastStep();

			WalsiScene currentScene = (WalsiScene) script.getLastStep().getScene();
			WalsiMethodProvider mp = new WalsiMethodProvider();
			Collection trees = currentScene.getTrees();

			MultiCriteriaThinner2ParameterLoader loader = new MultiCriteriaThinner2ParameterLoader();

			if(scenario == "3") {
				loader.load(thinning2);
			}else {
				loader.load(thinning);
			}

			step = script.evolve(new WalsiEvolutionParameters(cuttingCycle/2)); // mi-rotation avant la première coupe
			MultiCriteriaThinner2 thinner = new MultiCriteriaThinner2(loader); // not reproductible
			step = script.runIntervener(thinner, step);

			for(int i = 0; i < 3; i++) { // 4 cutting cycle
				step = script.evolve(new WalsiEvolutionParameters(cuttingCycle)); 
				step = script.runIntervener(thinner, step);
			}

			step = script.evolve(new WalsiEvolutionParameters(cuttingCycle/2)); // mi-rotation avant la dernière coupe

			// Save to reopen in gui mode (for demonstration)
			String scenarioName = "MIX" + scenario;

			String out = inputDirectory + "out\\" + scenarioName + ".prj";
			Engine.getInstance().processSaveAsProject(script.getProject(), out);

			System.out.println("Wrote project file: " + out);

			// export data
			currentScene = (WalsiScene) step.getScene();
			WalsiByClassExport export = new WalsiByClassExport();
			export.prepareExport(currentScene);
			export.save(inputDirectory + "out\\" + scenarioName + "_classExport.txt");

		}
	}

}
