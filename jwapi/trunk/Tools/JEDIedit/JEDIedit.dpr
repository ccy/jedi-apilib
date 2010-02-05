{==============================================================================}
{   JEDIedit command line text file editing program, file "JEDIedit.dpr"       }
{                                                                              }
{   Copyright (C) 2010 Conrad T. Pino.  All rights reserved.                   }
{------------------------------------------------------------------------------}
{   The contents of this file are subject to the Mozilla Public License        }
{   Version 1.1 (the "License"); you may not use this file except in           }
{   compliance with the License. You may obtain a copy of the License          }
{   at http://www.mozilla.org/MPL/                                             }
{                                                                              }
{   Software distributed under the License is distributed on an "AS IS"        }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See        }
{   the License for the specific language governing rights and limitations     }
{   under the License.                                                         }
{                                                                              }
{   The Original Code is "JEDIedit.dpr".                                       }
{                                                                              }
{   The Initial Developer of the Original Code is Conrad T. Pino.              }
{                                                                              }
{   Contributor(s): (none).                                                    }
{                                                                              }
{   Alternatively, the contents of this file may be used under the terms of    }
{   the GPL license (the "GNU General Public License"), in which case the      }
{   provisions of the GPL are applicable instead of those above. If you wish   }
{   to allow use of your version of this file only under the terms of the GPL  }
{   and not to allow others to use your version of this file under the MPL,    }
{   indicate your decision by deleting the provisions above and replace them   }
{   with the notice and other provisions required by the GPL. If you do not    }
{   delete the provisions above, a recipient may use your version of this      }
{   file under either the MPL or the GPL.                                      }
{------------------------------------------------------------------------------}
{   This program is free software: you can redistribute it and/or modify it    }
{   under the terms of the GNU General Public License as published by the      }
{   Free Software Foundation, either version 3 of the License, or (at your     }
{   option) any later version.                                                 }
{                                                                              }
{   This program is distributed in the hope that it will be useful, but        }
{   WITHOUT ANY WARRANTY; without even the implied warranty of                 }
{   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          }
{   General Public License for more details.                                   }
{                                                                              }
{   You should have received a copy of the GNU General Public License along    }
{   with this program.  If not, see <http://www.gnu.org/licenses/>.            }
{                                                                              }
{   Alternatively, the contents of this file may be used under the terms of    }
{   the MPL license (the "Mozilla Public License"), in which case the          }
{   provisions of the MPL are applicable instead of those above. If you wish   }
{   to allow use of your version of this file only under the terms of the MPL  }
{   and not to allow others to use your version of this file under the GPL,    }
{   indicate your decision by deleting the provisions above and replace them   }
{   with the notice and other provisions required by the MPL. If you do not    }
{   delete the provisions above, a recipient may use your version of this      }
{   file under either the GPL or the MPL.                                      }
{==============================================================================}
program JEDIedit;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JEditUnit in 'JEditUnit.pas';

begin
  ExitCode := RunProgram;
end.
