!>@file   set_ucd_extensions.f90
!!@brief  module set_ucd_extensions
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      character(len = kchara) function add_ucd_extension(file_header)
!!                put ".inp" at the end
!!      character(len = kchara) function add_udt_extension(file_header)
!!                put ".udt" at the end
!!      character(len = kchara) function add_grd_extension(file_header)
!!                put ".grd" at the end
!!
!!      character(len = kchara) function add_vtk_extension(file_header)
!!                put ".vtk" at the end
!!      character(len = kchara) function add_vtd_extension(file_header)
!!                put ".vtd" at the end
!!      character(len = kchara) function add_vtg_extension(file_header)
!!                put ".vtg" at the end
!!@endverbatim
!!
!!@n @param file_header      file prefix
!!
      module set_ucd_extensions
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_ucd_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_ucd_extension,1011) trim(file_header)
 1011 format (a,".inp")
!
      end function add_ucd_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_udt_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_udt_extension,1011) trim(file_header)
 1011 format (a,".udt")
!
      end function add_udt_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_grd_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
      write(add_grd_extension,1011) trim(file_header)
 1011 format (a,".grd")
!
      end function add_grd_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_vtk_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_vtk_extension,1011) trim(file_header)
 1011 format (a,".vtk")
!
      end function add_vtk_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_vtd_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_vtd_extension,1011) trim(file_header)
 1011 format (a,".vtd")
!
      end function add_vtd_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_vtg_extension(file_header)
!
      character(len=kchara), intent(in) :: file_header
!
       write(add_vtg_extension,1011) trim(file_header)
 1011 format (a,".vtg")
!
      end function add_vtg_extension
!
!-----------------------------------------------------------------------
!
      end module set_ucd_extensions
