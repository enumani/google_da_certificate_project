-- Select the data that we are going to use
select location, date, total_cases, new_cases, total_deaths, population
from Project..['covid-deaths$']
order by 1,2

-- Looking at total cases vs total deaths
-- Shows the likelyhood of dying if you contact Covid
select location, date, total_cases, total_deaths, (CONVERT(float, total_deaths) / NULLIF(CONVERT(float, total_cases), 0))*100 as DeathPercentage
from Project..['covid-deaths$']
where location='Albania'
order by 1,2

-- Looking at total cases vs population
-- Shows what percentage of population got Covid
select location, date, total_cases, population, (CONVERT(float, total_cases) / NULLIF(CONVERT(float, population), 0))*100 as CasesPercentage
from Project..['covid-deaths$']
order by 1,2

-- Looking at countries with highest infection rate compared to population
select location, population, max(total_cases) as HighestInfectionCount, (MAX(total_cases)/population)*100 as CasesPercentage
from Project..['covid-deaths$']
group by location, population
order by CasesPercentage desc

-- Countries with highest death count per population
select location, max(cast(total_deaths as int)) as TotalDeathCount
from Project..['covid-deaths$']
where continent is not null
group by location, population
order by TotalDeathCount desc

-- LET'S BREAK THINGS DOWN BY CONTINENT
select location, max(cast(total_deaths as int)) as TotalDeathCount
from Project..['covid-deaths$']
where continent is null
group by location
order by TotalDeathCount desc

-- Showing continents with the highest death count per population
select continent, max(cast(total_deaths as int)) as TotalDeathCount
from Project..['covid-deaths$']
where continent is not null
group by continent
order by TotalDeathCount desc

-- GLOBAL NUMBERS
select date, SUM(new_cases) as TotalCases, SUM(new_deaths) as TotalDeaths,SUM(new_deaths)/nullif(SUM(new_cases),0)*100,0 as DeathPercentage
from Project..['covid-deaths$']
where continent is not null
group by date
order by 1,2

-- Looking at total population vs vaccinations
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(convert(bigint,vac.new_vaccinations)) OVER (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated,
(RollingPeopleVaccinated/population)*100
from Project..['covid-deaths$'] dea
join Project..['covid-vaccinations$'] vac
on dea.location=vac.location
and dea.date=vac.date
where dea.continent is not null
order by 2,3

-- Use CTE
with PopvsVac (continent, location, date, population, new_vaccinations, RollingPeopleVaccinated)
as (
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(convert(bigint,vac.new_vaccinations)) OVER (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
from Project..['covid-deaths$'] dea
join Project..['covid-vaccinations$'] vac
on dea.location=vac.location
and dea.date=vac.date
where dea.continent is not null
and dea.location='Albania'
)
select *, (RollingPeopleVaccinated/population)*100
from PopvsVac

-- Temp Table
drop table if exists #PercentPopulationVaccinated
create table #PercentPopulationVaccinated
(continent nvarchar(255), location nvarchar(255), date datetime, population numeric, new_vaccinations numeric, RollingPeopleVaccinated numeric)
insert into #PercentPopulationVaccinated
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(convert(bigint,vac.new_vaccinations)) OVER (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
from Project..['covid-deaths$'] dea
join Project..['covid-vaccinations$'] vac
on dea.location=vac.location
and dea.date=vac.date
where dea.continent is not null

select *, (RollingPeopleVaccinated/population)*100
from #PercentPopulationVaccinated

-- Create view to store data for later visualizations
use Project
go
create view PercentPopulationVaccinated1 as
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
SUM(convert(bigint,vac.new_vaccinations)) OVER (partition by dea.location order by dea.location, dea.date) as RollingPeopleVaccinated
from Project..['covid-deaths$'] dea
join Project..['covid-vaccinations$'] vac
on dea.location=vac.location
and dea.date=vac.date
where dea.continent is not null

select *
from PercentPopulationVaccinated1