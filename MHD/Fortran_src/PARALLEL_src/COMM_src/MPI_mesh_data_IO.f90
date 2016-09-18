!>@file   MPI_mesh_data_IO.f90
!!@brief  module MPI_mesh_data_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine mpi_write_geometry_data                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO)
!!      subroutine mpi_read_geometry_data                             &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!@endverbatim
!
      module MPI_mesh_data_IO
!
      use m_precision
      use m_constants
!
      use t_calypso_mpi_IO_param
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      implicit  none
!
      private :: mpi_write_geometry_info, mpi_write_element_info
      private :: mpi_read_number_of_node, mpi_read_geometry_info
      private :: mpi_read_number_of_element, mpi_read_element_info
!
      type(calypso_MPI_IO_params), private :: IO_param
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_write_geometry_data                                &
     &         (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO)
!
!      use MPI_domain_data_IO
      use m_fem_mesh_labels
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: nprocs_in, id_rank
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call calypso_mpi_seek_write_head_c                                &
     &   (id_file, ioff_gl, hd_fem_para())
!      call mpi_write_domain_info(IO_param, mesh_IO%nod_comm)
      call dealloc_istack_merge(IO_param)
!
      call calypso_mpi_seek_write_head_c                                &
     &   (id_file, ioff_gl, hd_fem_node())
!      call mpi_write_geometry_info                                     &
!     &   (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO%node)
      call calypso_mpi_seek_write_head_c                                &
     &   (id_file, ioff_gl, hd_fem_elem())
!      call mpi_write_element_info                                      &
!     &   (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO%ele)
!
      call calypso_mpi_seek_write_head_c                                &
     &   (id_file, ioff_gl, hd_fem_import())
!      call mpi_write_import_data(IO_param, mesh_IO%nod_comm)
      call calypso_mpi_seek_write_head_c                                &
     &   (id_file, ioff_gl, hd_fem_export())
!      call mpi_write_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine mpi_write_geometry_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_data                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO)
!
      use m_error_IDs
      use MPI_domain_data_IO
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_read_domain_info(IO_param, mesh_IO%nod_comm)
!
      call mpi_read_number_of_node                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO%node)
      call mpi_read_geometry_info                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO%node)
!
!  ----  read element data -------
!
      call mpi_read_number_of_element                                   &
     &   (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO%ele)
      call mpi_read_element_info                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, mesh_IO%ele)
!
! ----  import & export 
!
      call mpi_read_import_data(IO_param, mesh_IO%nod_comm)
      call mpi_read_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine mpi_read_geometry_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_geometry_info                                &
     &         (id_file, nprocs_in, id_rank, ioff_gl, nod_IO)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
      type(node_data), intent(inout) :: nod_IO
!
!
      call mpi_write_one_integer                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nod_IO%numnod)
      call mpi_write_one_integer                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nod_IO%internal_node)
!
      call set_istack_4_parallell_data(nod_IO%numnod, IO_param)
!
      call mpi_write_int8_vector                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nod_IO%numnod, nod_IO%inod_global, IO_param%istack_merged)
      call mpi_write_2d_vector(id_file, nprocs_in, id_rank, ioff_gl,    &
     &    nod_IO%numnod, ithree, nod_IO%xx, IO_param%istack_merged)
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine mpi_write_geometry_info
!
!------------------------------------------------------------------
!
      subroutine mpi_write_element_info                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl, ele_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: num
!
!
      call mpi_write_one_integer                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, ele_IO%numele)
!
      call set_istack_4_parallell_data(ele_IO%numele, IO_param)
      call mpi_write_int_vector(id_file, nprocs_in, id_rank, ioff_gl,   &
     &    ele_IO%numele, ele_IO%elmtyp, IO_param%istack_merged)
      call mpi_write_int8_vector                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ele_IO%numele, ele_IO%iele_global, IO_param%istack_merged)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call mul_istack_4_parallell_vect(ele_IO%nnod_4_ele, IO_param)
      call mpi_write_int_vector(id_file, nprocs_in, id_rank, ioff_gl,   &
     &    num, ele_IO%ie, IO_param%istack_merged)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine mpi_write_element_info
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_number_of_node                                &
     &         (id_file, nprocs_in, id_rank, ioff_gl, nod_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(node_data), intent(inout) :: nod_IO
!
!
      call mpi_read_one_integer                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nod_IO%numnod)
      call mpi_read_one_integer                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nod_IO%internal_node)
!
      end subroutine mpi_read_number_of_node
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_info                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl, nod_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(node_data), intent(inout) :: nod_IO
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call set_istack_4_parallell_data(nod_IO%numnod, IO_param)
!
      call mpi_read_int8_vector                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nod_IO%numnod, nod_IO%inod_global, IO_param%istack_merged)
      call mpi_read_2d_vector                                           &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nod_IO%numnod, ithree, nod_IO%xx, IO_param%istack_merged)
!
      end subroutine mpi_read_geometry_info
!
!------------------------------------------------------------------
!
      subroutine mpi_read_number_of_element                             &
     &         (id_file, nprocs_in, id_rank, ioff_gl, ele_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(element_data), intent(inout) :: ele_IO
!
!
      call mpi_read_one_integer                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, ele_IO%numele)
!
      end subroutine mpi_read_number_of_element
!
!------------------------------------------------------------------
!
      subroutine mpi_read_element_info                                  &
     &         (id_file, nprocs_in, id_rank, ioff_gl, ele_IO)
!
      use set_nnod_4_ele_by_type
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: num, i
!
!
      call alloc_element_types(ele_IO)
!
      call set_istack_4_parallell_data(ele_IO%numele, IO_param)
      call mpi_read_int_vector                                          &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ele_IO%numele, ele_IO%elmtyp, IO_param%istack_merged)
!
      ele_IO%nnod_4_ele = 0
      do i = 1, ele_IO%numele
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
        ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
      end do
!
      call alloc_ele_connectivity(ele_IO)
!
      call mpi_read_int8_vector                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ele_IO%numele, ele_IO%iele_global, IO_param%istack_merged)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call mul_istack_4_parallell_vect(ele_IO%nnod_4_ele, IO_param)
      call mpi_read_int_vector                                          &
     &   (id_file, nprocs_in, id_rank, ioff_gl, num, ele_IO%ie,         &
     &    IO_param%istack_merged)
!
      end subroutine mpi_read_element_info
!
!------------------------------------------------------------------
!
      end module MPI_mesh_data_IO
