!gz_merged_udt_vtk_file_IO.f90
!------- module gz_merged_udt_vtk_file_IO ---------------------
!
!        programmed by H.Matsui on July, 2006
!
!>@file  gz_merged_udt_vtk_file_IO.f90
!!       module gz_merged_udt_vtk_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in July, 2006
!!@n      Modified  in May, 2015
!
!> @brief Output merged VTK or UCD  file usging MPI-IO
!!
!!@verbatim
!!      subroutine write_gz_merged_ucd_file(istep, ucd, m_ucd)
!!      subroutine write_gz_merged_udt_file(istep, ucd, m_ucd)
!!      subroutine write_gz_merged_grd_file(ucd, m_ucd)
!!
!!      subroutine write_gz_merged_vtk_file(istep, ucd, m_ucd)
!!      subroutine write_gz_merged_vtk_phys(istep, ucd, m_ucd)
!!      subroutine write_gz_merged_vtk_grid(ucd, m_ucd)
!!@endverbatim
!
      module gz_merged_udt_vtk_file_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_field_file_format
!
      use t_ucd_data
!
      use set_ucd_file_names
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_ucd_file(istep, ucd, m_ucd)
!
      use gz_ucd_file_MPI_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      call set_single_ucd_file_name(ucd%file_prefix,                    &
     &      iflag_ucd_gz, istep, gzip_name)
!
      if(my_rank .eq. 0) then
        write(*,*) 'gzipped UCD data by MPI-IO: ', trim(gzip_name)
      end if
!
      call gz_write_ucd_file_mpi(gzip_name, ucd, m_ucd)
!
      end subroutine write_gz_merged_ucd_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_udt_file(istep, ucd, m_ucd)
!
      use gz_ucd_file_MPI_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
      call set_single_ucd_file_name(ucd%file_prefix,                    &
     &      iflag_udt_gz, istep, gzip_name)
!
      if(my_rank .eq. 0) then
        write(*,*) 'gzipped UCD field by MPI-IO: ', trim(gzip_name)
      end if
!
      call gz_write_ucd_phys_mpi(gzip_name, ucd, m_ucd)
!
      end subroutine write_gz_merged_udt_file
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_grd_file(ucd, m_ucd)
!
      use gz_ucd_file_MPI_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      call set_single_grd_file_name(ucd%file_prefix, iflag_udt_gz,      &
     &      gzip_name)
!
      if(my_rank .eq. 0) then
        write(*,*) 'gzipped UCD grid by MPI-IO: ', trim(gzip_name)
      end if
!
      call gz_write_ucd_grid_mpi(gzip_name, ucd, m_ucd)
!
      end subroutine write_gz_merged_grd_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_gz_merged_vtk_file(istep, ucd, m_ucd)
!
      use gz_vtk_file_MPI_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      call set_single_ucd_file_name(ucd%file_prefix, iflag_vtk_gz,      &
     &    istep, gzip_name)
!
     if(my_rank .eq. 0) then
        write(*,*) 'gzipped VTK data by MPI-IO: ', trim(gzip_name)
      end if
      call calypso_mpi_barrier
!
      call gz_write_vtk_file_mpi(gzip_name, ucd, m_ucd)
!
      end subroutine write_gz_merged_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_merged_vtk_phys(istep, ucd, m_ucd)
!
      use gz_vtk_file_MPI_IO
!
      integer(kind = kint), intent(in) :: istep
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      call set_single_ucd_file_name(ucd%file_prefix, iflag_vtd_gz,      &
     &      istep, gzip_name)
!
     if(my_rank .eq. 0) then
        write(*,*) 'gzipped VTK field by MPI-IO: ', trim(gzip_name)
      end if
      call calypso_mpi_barrier
!
      call gz_write_vtk_phys_mpi(gzip_name, ucd, m_ucd)
!
      end subroutine write_gz_merged_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_merged_vtk_grid(ucd, m_ucd)
!
      use gz_vtk_file_MPI_IO
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      character(len=kchara) :: gzip_name
!
!
      call set_single_grd_file_name(ucd%file_prefix, iflag_vtd_gz,      &
     &    gzip_name)
!
     if(my_rank .eq. 0) then
        write(*,*) 'gzipped VTK grid by MPI-IO: ', trim(gzip_name)
      end if
      call calypso_mpi_barrier
!
      call gz_write_vtk_grid_mpi(gzip_name, ucd, m_ucd)
!
      end subroutine write_gz_merged_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module gz_merged_udt_vtk_file_IO
