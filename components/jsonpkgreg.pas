{
 /***************************************************************************
                            JSONPkgReg.pas
                            -------------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author:
    Andreas Frieß
	

  Abstract:
    Registration of the FCL-JSONDataSet components used by the IDE.
	
  THIS PACKAGE IS FOR DEMO USE ONLY
	
}
unit JSONPkgReg;

{$mode objfpc}{$H+}

interface
uses
  LazarusPackageIntf,
{$ifdef mswindows}
  ServiceManager,
{$endif}
  Classes, SysUtils, Process, DB, BufDataset, fpjsondataset;

procedure RegisterJSONComponents;

implementation

procedure RegisterDB;
begin
  RegisterComponents('Data Access',[TJSONDataSet]);
end;

procedure RegisterJSONComponents;
begin
  RegisterUnit('DB',@RegisterDB);
end;

initialization
  RegisterPackage('FCL', @RegisterJSONComponents);


end.

