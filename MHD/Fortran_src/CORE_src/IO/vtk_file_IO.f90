!>@file  vtk_file_IO.f90
!!       module vtk_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in March, 2007
!!@n      Modified by H. Matsui in July, 2013
!
!> @brief Output VTK file
!!
!!@verbatim
!!      subroutine write_parallel_vtk_file                              &
!!     &         (id_rank, nprocs, istep, file_prefix)
!!
!!      subroutine write_vtk_file(id_rank, file_name, ucd)
!!      subroutine write_vtk_phys(id_rank, file_name, ucd)
!!      subroutine write_vtk_grid(id_rank, file_name, ucd)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_vtk_file(id_rank, file_name, ucd)
!!      subroutine read_vtk_phys(id_rank, file_name, ucd)
!!      subroutine read_vtk_grid(id_rank, file_name, ucd)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module vtk_file_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use vtk_data_IO
      use t_ucd_data
!
      implicit none
!
!>      file ID for VTK file
      integer(kind = kint), parameter, private :: id_vtk_file = 16
!
      private :: read_ucd_mesh_by_vtk, read_ucd_field_by_vtk
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_parallel_vtk_file                                &
     &         (id_rank, nprocs, istep, file_prefix)
!
      use set_parallel_file_name
      use set_ucd_file_names
      use set_ucd_extensions
!
      character(len=kchara), intent(in) :: file_prefix
      integer, intent(in) :: id_rank, nprocs
      integer(kind = kint), intent(in) :: istep
!
      character(len=kchara)  :: file_name, fname_tmp
      character(len=kchara) :: fname_nodir
      integer :: ip
!
!
      if(id_rank .gt. 0) return
!
      fname_nodir = delete_directory_name(file_prefix)
      fname_tmp =   add_int_suffix(istep, file_prefix)
      file_name =   add_pvtk_extension(fname_tmp)
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
        file_name = set_parallel_vtk_file_name(fname_nodir, ip, istep)
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
!
      subroutine write_vtk_file(id_rank, file_name, ucd)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK file: ', trim(file_name)
!
      open(id_vtk_file, file=file_name, form='formatted',               &
     &     status ='unknown')
!
      call write_vtk_mesh(id_vtk_file, ucd%nnod,                        &
     &    ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie)
      call write_vtk_data(id_vtk_file, ucd%nnod, ucd%num_field,         &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      close(id_vtk_file)
!
      end subroutine write_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_phys(id_rank, file_name, ucd)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK fields: ', trim(file_name)
!
      open(id_vtk_file, file=file_name, form='formatted',               &
     &     status ='unknown')
!
      call write_vtk_data                                               &
     &   (id_vtk_file, ucd%nnod, ucd%num_field, ucd%ntot_comp,          &
     &    ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      close(id_vtk_file)
!
      end subroutine write_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_grid(id_rank, file_name, ucd)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK mesh: ', trim(file_name)
!
      open(id_vtk_file, file=file_name, form='formatted',               &
     &     status ='unknown')
      call write_vtk_mesh(id_vtk_file, ucd%nnod,                        &
     &    ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie)
      close(id_vtk_file)
!
      end subroutine write_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_vtk_file(id_rank, file_name, ucd)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii VTK file: ', trim(file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_ucd_mesh_by_vtk(id_vtk_file, ucd)
      call read_ucd_field_by_vtk(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine read_vtk_file
!
!-----------------------------------------------------------------------
!
      subroutine read_vtk_phys(id_rank, file_name, ucd)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii VTK fields: ', trim(file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_ucd_field_by_vtk(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine read_vtk_phys
!
!-----------------------------------------------------------------------
!
      subroutine read_vtk_grid(id_rank, file_name, ucd)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii VTK mesh: ', trim(file_name)
!
      open(id_vtk_file, file=file_name,                                 &
     &    form='formatted', status ='old')
      call read_ucd_mesh_by_vtk(id_vtk_file, ucd)
      close(id_vtk_file)
!
      end subroutine read_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ucd_mesh_by_vtk(id_vtk, ucd)
!
      integer(kind = kint), intent(in) ::  id_vtk
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: inod, iele
!
!
      call read_vtk_node_head(id_vtk, ucd%nnod)
      call allocate_ucd_node(ucd)
!
      call read_vtk_each_field                                          &
     &   (id_vtk, ucd%nnod, ithree, ucd%nnod, ucd%xx)
!
!$omp parallel do
      do inod = 1, ucd%nnod
        ucd%inod_global(inod) = inod
      end do
!$omp end parallel do
!
      call read_vtk_connect_head(id_vtk, ucd%nele, ucd%nnod_4_ele)
      call allocate_ucd_ele(ucd)
!
      call read_vtk_connect_data                                        &
     &   (id_vtk, ucd%nele, ucd%nnod_4_ele, ucd%nele, ucd%ie)
      call read_vtk_cell_type(id_vtk, ucd%nele)
!
!$omp parallel do
      do iele = 1, ucd%nele
        ucd%iele_global(iele) = iele
      end do
!$omp end parallel do
!
      end subroutine read_ucd_mesh_by_vtk
!
!-----------------------------------------------------------------------
!
      subroutine read_ucd_field_by_vtk(id_vtk, ucd)
!
      integer(kind = kint), intent(in) ::  id_vtk
      type(ucd_data), intent(inout) :: ucd
!
      type(ucd_data) :: tmp
!
      integer(kind = kint) :: iflag_end, ncomp_field
      character(len=kchara)  :: field_name
      real(kind = kreal), allocatable :: d_tmp(:,:)
!
!
      call read_vtk_fields_head(id_vtk, ucd%nnod)
!
      tmp%nnod =      ucd%nnod
      ucd%num_field = 0
      ucd%ntot_comp = 0
      call allocate_ucd_phys_name(ucd)
      call allocate_ucd_phys_data(ucd)
!
      do
        call read_vtk_each_field_head                                   &
     &    (id_vtk, iflag_end, ncomp_field, field_name)
        if(iflag_end .ne. izero) exit
!
        tmp%num_field = ucd%num_field
        tmp%ntot_comp = ucd%ntot_comp
        call allocate_ucd_phys_name(tmp)
        call allocate_ucd_phys_data(tmp)
!
        call append_new_ucd_field_name(field_name, ncomp_field,         &
     &      tmp, ucd)
!
        allocate(d_tmp(ucd%nnod,ncomp_field))
!
        call read_vtk_each_field                                        &
     &     (id_vtk, ucd%nnod, ncomp_field, ucd%nnod, d_tmp(1,1))
        call append_new_ucd_field_data(ncomp_field, d_tmp, tmp, ucd)
!
        deallocate(d_tmp)
        call deallocate_ucd_data(tmp)
!
        iflag_end = izero
      end do
!
      end subroutine read_ucd_field_by_vtk
!
!-----------------------------------------------------------------------
!
      end module vtk_file_IO
