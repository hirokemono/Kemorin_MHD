!>@file   write_ucd_to_vtk_file.f90
!!@brief  module write_ucd_to_vtk_file
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on July, 2006
!!@n       Modified by H.Matsui on March, 2013
!
!>@brief Output FEM field data to distributed VTK file
!!
!!@verbatim
!!      subroutine write_parallel_vtk_file(my_rank, istep)
!!
!!      subroutine write_udt_data_2_vtk_file(my_rank, istep)
!!      subroutine write_udt_data_2_vtk_phys(my_rank, istep)
!!      subroutine write_udt_data_2_vtk_grid(my_rank)
!!@endverbatim
!!
!!@param my_rank    subdomain ID
!!@param istep      Step number for VTK data
!
      module write_ucd_to_vtk_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_field_file_format
      use m_ucd_data
      use set_ucd_file_names
!
      implicit none
!
!>      file ID for VTK file
      integer(kind = kint), parameter, private :: id_vtk_file = 16
!
!-----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_parallel_vtk_file(my_rank, nprocs, istep)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, nprocs, istep
!
      character(len=kchara)  :: file_name, fname_tmp
      character(len=kchara) :: fname_nodir
      integer(kind = kint) :: ip
!
!
      if(my_rank .gt. 0) return
!
      call delete_directory_name(ucd_header_name, fname_nodir)
      call add_int_suffix(istep, ucd_header_name, fname_tmp)
      call add_pvtk_extension(fname_tmp, file_name)
!
      write(*,*) 'Write parallel VTK file: ', trim(file_name)
      open(id_vtk_file, file=file_name)
!
      write(id_vtk_file,'(a)') '<File version="pvtk-1.0"'
      write(id_vtk_file,'(a)')                                          &
     &     '       dataType="vtkUnstructuredGrid"'
      write(id_vtk_file,'(a,i6,a)')                                     &
     &     '       numberOfPieces="', nprocs, '" >'
      do ip = 0, nprocs-1
        call set_parallel_ucd_file_name(fname_nodir, iflag_vtk,         &
     &      ip, istep, file_name)
        write(id_vtk_file,'(3a)') '   <Piece fileName="',               &
     &                       trim(file_name), '" />'
      end do
      write(id_vtk_file,'(a)') '</File>'
!
      close(id_vtk_file)
!
      end subroutine write_parallel_vtk_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_udt_data_2_vtk_file(my_rank, istep)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_vtk,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK file: ', trim(file_name)
!
      call write_vtk_file(file_name, id_vtk_file,                       &
     &    nnod_ucd, nele_ucd, nnod_4_ele_ucd, xx_ucd, ie_ucd,           &
     &    num_field_ucd, ntot_comp_ucd, num_comp_ucd, phys_name_ucd,    &
     &    d_nod_ucd)
!
      end subroutine write_udt_data_2_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_udt_data_2_vtk_phys(my_rank, istep)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_vtd,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK fields: ', trim(file_name)
!
      call write_vtk_phys(file_name, id_vtk_file,                       &
     &    nnod_ucd, num_field_ucd, ntot_comp_ucd,                       &
     &    num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      end subroutine write_udt_data_2_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_udt_data_2_vtk_grid(my_rank)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank
      character(len=kchara) :: file_name
!
!
      call set_parallel_grd_file_name(ucd_header_name, iflag_vtd,       &
     &    my_rank, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK mesh: ', trim(file_name)
!
      call write_vtk_grid(file_name, id_vtk_file,                       &
     &    nnod_ucd, nele_ucd, nnod_4_ele_ucd, xx_ucd, ie_ucd)
!
      end subroutine write_udt_data_2_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module write_ucd_to_vtk_file
