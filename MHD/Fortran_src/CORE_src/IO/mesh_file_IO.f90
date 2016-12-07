!> @file  mesh_file_IO.f90
!!      module mesh_file_IO
!!
!! @author  H. Matsui
!! @date Programmed in Apr., 2006
!
!> @brief ASCII mesh file IO
!!
!!@verbatim
!!      subroutine read_mesh_file(my_rank_IO, fem_IO, ierr)
!!        type(mesh_data), intent(inout) :: fem_IO
!!
!!      subroutine read_mesh_geometry(my_rank_IO, mesh_IO, ierr)
!!      subroutine read_node_size(my_rank_IO, mesh_IO, ierr)
!!      subroutine read_geometry_size(my_rank_IO, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine write_mesh_file(my_rank_IO, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!
!!      subroutine write_node_position_sph(my_rank_IO, mesh_IO)
!!      subroutine write_node_position_cyl(my_rank_IO, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!@endverbatim
!
      module mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
      use t_mesh_data
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                   :: mesh_sph_def_head = 'mesh/node_sph'
      character(len=kchara), parameter, private                         &
     &                   :: mesh_cyl_def_head = 'mesh/node_cyl'
      character(len=kchara) :: mesh_sph_file_head = mesh_sph_def_head
      character(len=kchara) :: mesh_cyl_file_head = mesh_cyl_def_head
!
!   mesh file code
      integer(kind = kint), parameter ::  input_file_code = 14
!
      private :: input_file_code
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_file(my_rank_IO, fem_IO, ierr)
!
      use mesh_data_IO
!
      integer(kind=kint), intent(in) :: my_rank_IO
!
      type(mesh_data), intent(inout) :: fem_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!
      call read_geometry_data(input_file_code, my_rank_IO,              &
     &    fem_IO%mesh, ierr)
      call read_mesh_groups(input_file_code, fem_IO%group)
      close(input_file_code)
!
      end subroutine read_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_mesh_geometry(my_rank_IO, mesh_IO, ierr)
!
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_geometry_data(input_file_code, my_rank_IO,              &
     &   mesh_IO, ierr)
      close(input_file_code)
!
!
      end subroutine read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine read_node_size(my_rank_IO, mesh_IO, ierr)
!
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_num_node(input_file_code, my_rank_IO, mesh_IO, ierr)
      close(input_file_code)
!
!
      end subroutine read_node_size
!
!------------------------------------------------------------------
!
      subroutine read_geometry_size(my_rank_IO, mesh_IO, ierr)
!
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_num_node_ele                                            &
     &   (input_file_code, my_rank_IO, mesh_IO, ierr)
      close(input_file_code)
!
      end subroutine read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_mesh_file(my_rank_IO, fem_IO)
!
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_data), intent(inout) :: fem_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Write ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!
      call write_geometry_data(input_file_code, my_rank_IO, fem_IO%mesh)
      call write_mesh_groups(input_file_code, fem_IO%group)
!
      close(input_file_code)
!
      end subroutine write_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine write_node_position_sph(my_rank_IO, mesh_IO)
!
      use mesh_data_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call add_int_suffix                                               &
     &   (my_rank_IO, mesh_sph_file_head, mesh_file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Write ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call output_node_sph_geometry                                     &
         (input_file_code, my_rank_IO, mesh_IO)
      close(input_file_code)
!
      end subroutine write_node_position_sph
!
!  ---------------------------------------------------------------------
!
      subroutine write_node_position_cyl(my_rank_IO, mesh_IO)
!
      use mesh_data_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call add_int_suffix                                               &
     &   (my_rank_IO, mesh_cyl_file_head, mesh_file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Write ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call output_node_cyl_geometry                                     &
         (input_file_code, my_rank_IO, mesh_IO)
      close(input_file_code)
!
      end subroutine write_node_position_cyl
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_IO
