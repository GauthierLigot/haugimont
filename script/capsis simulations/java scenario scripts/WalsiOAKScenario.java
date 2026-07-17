/* 
 * The Walsi model.
 *
 * Copyright (C) January 2023: Violette Van Keymeulen (ULiege).
 * 
 * This file is part of the Walsi model and is NOT free software.
 * It is the property of its authors and must not be copied without their 
 * permission. 
 * It can be shared by the modellers of the Capsis co-development community 
 * in agreement with the Capsis charter (http://capsis.cirad.fr/capsis/charter).
 * See the license.txt file in the Capsis installation directory 
 * for further information about licenses in Capsis.
 */
package gbx.walsi.myscripts.haugimont;

import capsis.app.C4Script;
import capsis.kernel.Engine;
import capsis.kernel.Step;
import gbx.walsi.extension.ioformat.WalsiByClassExport;
import gbx.walsi.model.WalsiEvolutionParameters;
import gbx.walsi.model.WalsiInitialParameters;
import gbx.walsi.model.WalsiModel;
import gbx.walsi.model.WalsiScene;
import gbx.walsi.model.EA_Oak_Scenario.WalsiEAOakScenario;
import gbx.walsi.model.EA_Oak_Scenario.WalsiEAOakScenarioConfig;
import jeeb.lib.util.PathManager;

/**
 * A Walsi script used to simulate a reference scenario of an evenaged oak stand.
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

public class WalsiOAKScenario {

	public static void main(String[] args) throws Exception {


		String inputDirectory = "C:\\OneDrive\\OneDrive - Universite de Liege\\PROJECT\\HAUGIMONT\\4_ANALYSE ECONOMIQUE\\8_Simulations\\3_evenagedoakSardin\\";

		String inventory = inputDirectory + "inventory.inv";
		String species = PathManager.getDir ("data") + "/gbx/walsi/Walsi.species"; //inputDirectory + "Walsi.species";
		String climate = inputDirectory + "climate.txt";
		int cuttingCycle = 8; // years
		double[] regeCdoms = new double[] {180,200,220,240,260,280};
		double[] siteIndexes = {24,27.5};
		String[] harvestScenarios = {"clear-cut","shelterwood"};

		int simulationNbYears = 200;

		for(String harvestScenario : harvestScenarios) {
			
			boolean isShelterwood = harvestScenario == "shelterwood";

			for(double siteIndex : siteIndexes) {
				for(double regeCdom : regeCdoms) {

					C4Script script = new C4Script("gbx.walsi");

					WalsiInitialParameters ip = new WalsiInitialParameters(inventory, species, climate);
					WalsiEAOakScenarioConfig scenarioConfig = new WalsiEAOakScenarioConfig(cuttingCycle,regeCdom,siteIndex,isShelterwood);
					WalsiEAOakScenario scenario = new WalsiEAOakScenario(scenarioConfig);

					// initialization
					script.init(ip);
					WalsiModel model = (WalsiModel) script.getModel(); // fc-19.6.2025 Reproducibility
					Step step = script.getLastStep();

					step = script.evolve(new WalsiEvolutionParameters(simulationNbYears, scenario));

					WalsiScene currentScene = (WalsiScene) script.getLastStep().getScene();

					// project name
					String projectName = harvestScenario + "_cdom_" + (int) regeCdom + "_SI_" + (int) siteIndex;

					// Save to reopen in gui mode (for demonstration)
					String out = inputDirectory + "out\\" + projectName + ".prj";
					Engine.getInstance().processSaveAsProject(script.getProject(), out);
					System.out.println("Wrote project file: " + out);

					// export data
					WalsiByClassExport export = new WalsiByClassExport();
					export.prepareExport(currentScene);
					export.save(inputDirectory + "out\\" + projectName + ".txt");
				}
			}
		}
	}
}
