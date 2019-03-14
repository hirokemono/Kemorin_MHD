!>@file   MPI_write_single_mesh_file.f90
!!@brief  module MPI_write_single_mesh_file
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine mpi_write_merged_mesh_file                           &
!!     &         (num_pe, id_rank, mesh_file, mesh, group, dbl_nod)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(parallel_double_numbering), intent(in) :: dbl_nod
!!@endverbatim
!
      module MPI_write_single_mesh_file
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_mesh_data
!
      implicit none
!
      private :: mpi_write_merged_geometry_data
      private :: mpi_write_merged_geometry, mpi_write_merged_element
!
      private :: mpi_write_merged_grp, mpi_write_merged_surf_grp
!
! ----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_merged_mesh_file                             &
     &         (num_pe, id_rank, mesh_file, mesh, group, dbl_nod)
!
      use t_para_double_numbering
      use t_file_IO_parameter
      use m_machine_parameter
      use m_fem_mesh_labels
      use mesh_data_IO
      use set_mesh_file_names
      use MPI_ascii_data_IO
!
      integer, intent(in) :: num_pe, id_rank
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(parallel_double_numbering), intent(in) :: dbl_nod
!
      type(calypso_MPI_IO_params):: IO_param
      character(len=kchara) :: file_name
!
!
      file_name = set_mesh_file_name                                    &
     &      (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call mpi_write_merged_geometry_data(IO_param, mesh, dbl_nod)
      call mpi_write_merged_mesh_groups                                 &
     &   (mesh%node%istack_internod(my_rank),                           &
     &    mesh%ele%istack_interele(my_rank), IO_param, group)
!
      call close_mpi_file(IO_param)
!
      end subroutine mpi_write_merged_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_merged_geometry_data                         &
     &         (IO_param, mesh_IO, dbl_nod)
!
      use t_para_double_numbering
      use m_fem_mesh_labels
      use MPI_write_single_mesh_data
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(in) :: mesh_IO
      type(parallel_double_numbering), intent(in) :: dbl_nod
!
      character(len=1) :: chara_dat
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(izero))
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(izero))
      write(chara_dat,'(a1)') char(10)
      call mpi_write_charahead(IO_param, 1, chara_dat)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_node()), hd_fem_node())
      call mpi_write_merged_geometry(IO_param, mesh_IO%node)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_elem()), hd_fem_elem())
      call mpi_write_merged_element                                     &
     &   (IO_param, mesh_IO%node, mesh_IO%ele, dbl_nod)
!
      write(chara_dat,'(a1)') char(10)
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_import()), hd_fem_import())
      call mpi_write_charahead(IO_param, 1, chara_dat)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_export()), hd_fem_export())
      call mpi_write_charahead(IO_param, 1, chara_dat)
!
      end subroutine mpi_write_merged_geometry_data
!
!------------------------------------------------------------------
!
      subroutine mpi_write_merged_mesh_groups                           &
     &         (nshift8_node, nshift8_ele, IO_param, mesh_group_IO)
!
      use m_fem_mesh_labels
      use MPI_groups_IO
!
      integer(kind=kint_gl), intent(in) :: nshift8_node, nshift8_ele
      type(mesh_groups), intent(in) ::   mesh_group_IO
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!   write node group
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_nodgrp()), hd_fem_nodgrp())
      call mpi_write_merged_grp                                         &
     &   (nshift8_node, IO_param, mesh_group_IO%nod_grp)
!  write element group
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_elegrp()), hd_fem_elegrp())
      call mpi_write_merged_grp                                         &
     &   (nshift8_ele, IO_param, mesh_group_IO%ele_grp)
!  write surface group
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_sfgrp()), hd_fem_sfgrp())
      call mpi_write_merged_surf_grp                                    &
     &   (nshift8_ele, IO_param, mesh_group_IO%surf_grp)
!
      end subroutine mpi_write_merged_mesh_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_merged_geometry(IO_param, nod_IO)
!
      use MPI_ascii_data_IO
      use MPI_write_single_mesh_data
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
!
      integer(kind = kint_gl) :: num_item_l(2)
      integer(kind = kint_gl) :: istack_g(2)
!
!
      num_item_l(1) = nod_IO%numnod
      num_item_l(2) = nod_IO%internal_node
      call MPI_allREDUCE(num_item_l(1), istack_g(1), 2,                 &
     &    CALYPSO_GLOBAL_INT, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      call mpi_write_charahead(IO_param, len_multi_int_textline(itwo),  &
     &    multi_int8_textline(itwo, istack_g(1)))
!
      call mpi_write_merged_node_position(IO_param,                     &
     &    nod_IO%numnod, ithree, nod_IO%inod_global, nod_IO%xx)
!
      end subroutine mpi_write_merged_geometry
!
!------------------------------------------------------------------
!
      subroutine mpi_write_merged_element                               &
     &         (IO_param, node_IO, ele_IO, dbl_nod)
!
      use t_para_double_numbering
      use MPI_ascii_data_IO
      use MPI_integer_list_IO
      use MPI_write_single_mesh_data
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(in) :: ele_IO
      type(node_data), intent(in) :: node_IO
      type(parallel_double_numbering), intent(in) :: dbl_nod
!
      integer(kind = kint_gl) :: num_item_l(1)
      integer(kind = kint_gl) :: istack_g(1)
!
!
      num_item_l(1) = ele_IO%numele
      call MPI_allREDUCE(num_item_l(1), istack_g(1), 1,                 &
     &    CALYPSO_GLOBAL_INT, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      call mpi_write_charahead(IO_param, len_multi_int_textline(ione),  &
     &    multi_int8_textline(ione, istack_g(1)))
!
!      call mpi_write_num_of_data(IO_param, ele_IO%numele)
      call mpi_write_merged_element_type                                &
     &   (IO_param, iten, ele_IO%numele, ele_IO%elmtyp)
!
      call mpi_write_merged_ele_connect(IO_param,                       &
     &    ele_IO%numele, ele_IO%nnod_4_ele, ele_IO%iele_global,         &
     &    ele_IO%ie, ele_IO%istack_interele, node_IO%istack_internod,   &
     &    dbl_nod%nnod_local, dbl_nod%inod_local, dbl_nod%irank_home)
!
      end subroutine mpi_write_merged_element
!
!------------------------------------------------------------------
!
      subroutine mpi_write_merged_grp(nshift8, IO_param, group_IO)
!
      use MPI_ascii_data_IO
      use MPI_domain_data_IO
      use data_IO_to_textline
      use MPI_write_single_mesh_data
!
      integer(kind=kint_gl), intent(in) :: nshift8
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(group_data), intent(in) :: group_IO
!
      integer(kind = kint) :: i, ist, num
      integer(kind = kint_gl) :: num64
      integer(kind = kint_gl) :: num_item_l(group_IO%num_grp)
      integer(kind = kint_gl) :: istack_g(0:group_IO%num_grp)
      character(len=1) :: chara_dat
!
!
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(group_IO%num_grp))
!
      do i = 1, group_IO%num_grp
        num_item_l(i) = group_IO%istack_grp(i)                          &
     &                 - group_IO%istack_grp(i-1)
      end do
!
      num64 = int(group_IO%num_grp, KIND(num64))
      call calypso_mpi_allreduce_int8                                   &
     &   (num_item_l, istack_g(1), num64, MPI_SUM)
      istack_g(0) = 0
      do i = 1, group_IO%num_grp
        istack_g(i) = istack_g(i) + istack_g(i-1)
      end do
!
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(group_IO%num_grp),                     &
     &    int_stack8_textline(group_IO%num_grp, istack_g))
!
      do i = 1, group_IO%num_grp
        call mpi_write_charahead(IO_param,                              &
     &      len_one_word_textline(group_IO%grp_name(i)),                &
     &      one_word_textline(group_IO%grp_name(i)))
!
        if(istack_g(i) .le. istack_g(i-1)) then
          write(chara_dat,'(a1)') char(10)
          call mpi_write_charahead(IO_param, 1, chara_dat)
        else
          ist = group_IO%istack_grp(i-1) + 1
          num = group_IO%istack_grp(i) - group_IO%istack_grp(i-1)
          call mpi_write_grp_item                                       &
     &       (IO_param, ieight, num, group_IO%item_grp(ist), nshift8)
        end if
      end do
!
      end subroutine mpi_write_merged_grp
!
!------------------------------------------------------------------
!
      subroutine mpi_write_merged_surf_grp                              &
     &         (nshift8_ele, IO_param, surf_grp_IO)
!
      use MPI_ascii_data_IO
      use MPI_domain_data_IO
      use data_IO_to_textline
      use MPI_write_single_mesh_data
!
      integer(kind=kint_gl), intent(in) :: nshift8_ele
      type(surface_group_data), intent(in) :: surf_grp_IO
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: i, num
      integer(kind = kint_gl) :: num64
      integer(kind = kint_gl) :: num_item_l(surf_grp_IO%num_grp)
      integer(kind = kint_gl) :: istack_g(0:surf_grp_IO%num_grp)
      character(len=1) :: chara_dat
!
!
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(surf_grp_IO%num_grp))
!
      do i = 1, surf_grp_IO%num_grp
        num_item_l(i) = surf_grp_IO%istack_grp(i)                       &
     &                 - surf_grp_IO%istack_grp(i-1)
      end do

      num64 = int(surf_grp_IO%num_grp, KIND(num64))
      call calypso_mpi_allreduce_int8                                   &
     &   (num_item_l, istack_g(1), num64, MPI_SUM)
      istack_g(0) = 0
      do i = 1, surf_grp_IO%num_grp
        istack_g(i) = istack_g(i) + istack_g(i-1)
      end do
!
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(surf_grp_IO%num_grp),                  &
     &    int_stack8_textline(surf_grp_IO%num_grp, istack_g))
!
      do i = 1, surf_grp_IO%num_grp
        call mpi_write_charahead(IO_param,                              &
     &      len_one_word_textline(surf_grp_IO%grp_name(i)),             &
     &      one_word_textline(surf_grp_IO%grp_name(i)))
!
        if(istack_g(i) .le. istack_g(i-1)) then
          write(chara_dat,'(a1)') char(10)
          call mpi_write_charahead(IO_param, 1, chara_dat)
        else
          num = surf_grp_IO%istack_grp(i) - surf_grp_IO%istack_grp(i-1)
          call mpi_write_surf_grp_item(IO_param, ieight,                &
     &        surf_grp_IO%num_item, surf_grp_IO%istack_grp(i-1),        &
     &        num, surf_grp_IO%item_sf_grp, nshift8_ele)
        end if
      end do
!
      end subroutine mpi_write_merged_surf_grp
!
!------------------------------------------------------------------
!
      end module MPI_write_single_mesh_file
