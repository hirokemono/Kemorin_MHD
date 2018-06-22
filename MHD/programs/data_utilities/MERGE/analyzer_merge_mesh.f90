!>@file   analyzer_merge_mesh.f90
!!@brief  module analyzer_merge_mesh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_merge_mesh
!!      subroutine analyze_merge_mesh
!!@endverbatim
!
      module analyzer_merge_mesh
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_phys_constants
      use m_geometry_data_4_merge
!
      use t_mesh_data
      use t_calypso_mpi_IO_param
      use mpi_load_mesh_data
!
      implicit none
!
      type(mesh_geometry), save :: mesh_m
      type(mesh_groups), save :: group_m
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_merge_mesh
!
      use m_error_IDs
      use m_control_param_merge
      use m_control_data_4_merge
      use m_array_for_send_recv
!
      use load_mesh_data_4_merge
      use nod_phys_send_recv
      use const_element_comm_tables
      use const_mesh_information
!
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
!
      write(*,*) 'Simulation start: PE. ', my_rank
      if(my_rank .eq. 0) then
        write(*,*) ' Do you prepare folloing data???'
        write(*,*) ' original mesh data:  mesh/in.PE#'
        write(*,*) ' control data for this routine:  control_merge'
      end if
!
!   read control data
!
      call read_control_4_merge
      call set_control_4_merge(mgd_mesh1%num_pe)
      call set_control_4_newudt(mgd_mesh1%num_pe)
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           istep_start, istep_end, increment_step
!
!  set mesh data
!
      call mpi_input_mesh(merge_org_mesh_file, nprocs,                  &
     &    mesh_m, group_m, nnod_4_surf, nnod_4_edge)
      call set_nod_and_ele_infos(mesh_m%node, mesh_m%ele)
      call const_global_numnod_list(mesh_m%node)
      call const_global_numele_list(mesh_m%ele)
!
!  Initialize communicator
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh_m%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(mesh_m)
!
      end subroutine init_merge_mesh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_merge_mesh
!
      use t_para_double_numbering
      use m_phys_labels
      use m_control_param_merge
      use m_file_format_switch
      use set_field_to_restart
      use const_internal_mesh_data
!
      type(mesh_geometry), save :: new_mesh
      type(mesh_groups), save :: new_group
      type(parallel_double_numbering), save :: dbl_nod
!
!
      call alloc_double_numbering(mesh_m%node%numnod, dbl_nod)
      call set_para_double_numbering                                    &
     &   (mesh_m%node%internal_node, mesh_m%nod_comm, dbl_nod)
!
      call s_const_internal_mesh_data                                   &
     &   (mesh_m, group_m, new_mesh, new_group)
!
      merged_mesh_file%iflag_format = iflag_single
      call mpi_output_merged_mesh                                       &
     &   (merged_mesh_file, new_mesh, new_group, dbl_nod)
!
      call dealloc_double_numbering(dbl_nod)
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_merge_mesh
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_output_merged_mesh(mesh_file, mesh, group, dbl_nod)
!
      use t_para_double_numbering
      use mesh_MPI_IO_select
      use set_element_data_4_IO
      use copy_mesh_structures
      use load_mesh_data
!
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(parallel_double_numbering), intent(in) :: dbl_nod
!
!
      write(*,*) 'mesh%node%istack_numnod', mesh%node%istack_numnod
      write(*,*) 'mesh%node%istack_internod', mesh%node%istack_internod
      write(*,*) 'mesh%ele%istack_numele', mesh%ele%istack_numele
      write(*,*) 'mesh%ele%istack_interele', mesh%ele%istack_interele
!
!       save mesh information
      call mpi_write_merged_mesh_file                                   &
     &   (nprocs, my_rank, mesh_file, mesh, group, dbl_nod)
!
      end subroutine mpi_output_merged_mesh
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_merged_mesh_file                             &
     &         (nprocs_in, my_rank_IO, mesh_file, mesh, group, dbl_nod)
!
      use t_para_double_numbering
      use m_machine_parameter
      use m_fem_mesh_labels
      use mesh_data_IO
      use mesh_file_name_by_param
      use MPI_ascii_data_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(parallel_double_numbering), intent(in) :: dbl_nod
!
      type(calypso_MPI_IO_params):: IO_param
      character(len=kchara) :: file_name
!
!
      call set_mesh_file_name_by_param(mesh_file, my_rank_IO, file_name)
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Write ascii mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
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
      use MPI_domain_data_IO
      use MPI_node_geometry_IO
      use MPI_element_connect_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
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
      call mpi_write_charahead(IO_param, ione, chara_dat)
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
      call mpi_write_charahead(IO_param, ione, chara_dat)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_export()), hd_fem_export())
      call mpi_write_charahead(IO_param, ione, chara_dat)
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
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
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
!
      subroutine mpi_write_merged_geometry(IO_param, nod_IO)
!
      use MPI_ascii_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
      integer(kind = kint_gl) :: num_item_l(2)
      integer(kind = kint_gl) :: istack_g(2)
!
!
      num_item_l(1) = nod_IO%numnod
      num_item_l(2) = nod_IO%internal_node
      call MPI_allREDUCE(num_item_l(1), istack_g(1), itwo,              &
     &    CALYPSO_GLOBAL_INT, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      call mpi_write_charahead(IO_param, len_multi_int_textline(itwo),  &
     &    multi_int8_textline(itwo, istack_g(1)))
!
      call mpi_write_merged_node_position(IO_param,                     &
     &    nod_IO%numnod, ithree, nod_IO%inod_global, nod_IO%xx)
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine mpi_write_merged_geometry
!
!------------------------------------------------------------------
!
      subroutine mpi_write_merged_node_position                         &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      use m_calypso_mpi_IO
      use data_IO_to_textline
      use MPI_ascii_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = kint) :: i, led, ilength
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = len_int8_and_vector_textline(numdir)
      led = nnod * len_int8_and_vector_textline(numdir)
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nnod .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nnod
          xx_tmp(1:numdir) = xx(i,1:numdir)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        int8_and_vector_textline(id_global(i), numdir, xx_tmp))
        end do
      end if
      call calypso_mpi_barrier
!
      end subroutine mpi_write_merged_node_position
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_merged_element                               &
     &         (IO_param, node_IO, ele_IO, dbl_nod)
!
      use t_para_double_numbering
      use MPI_ascii_data_IO
      use MPI_integer_list_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
      type(node_data), intent(inout) :: node_IO
      type(parallel_double_numbering), intent(in) :: dbl_nod
!
      integer(kind = kint_gl) :: num_item_l(1)
      integer(kind = kint_gl) :: istack_g(1)
!
!
      num_item_l(1) = ele_IO%numele
      call MPI_allREDUCE(num_item_l(1), istack_g(1), ione,              &
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
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine mpi_write_merged_element
!
!------------------------------------------------------------------
!
      subroutine mpi_write_merged_element_type                          &
     &         (IO_param, ncolumn, num, int_dat)
!
      use MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, loop, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      led = izero
      if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_6digit_line(ncolumn) * loop                     &
     &       + len_multi_6digit_line(nrest)
      end if
!
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(num .gt. 0) then
        do i = 0, (num-1)/ncolumn - 1
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &        len_multi_6digit_line(ncolumn),                           &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_6digit_line(nrest),                               &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)))
      end if
!
      end subroutine mpi_write_merged_element_type
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_merged_ele_connect(IO_param,                 &
     &          nele, nnod_4_ele, id_global, ie, istack_interele,       &
     &          istack_internod, nnod_local, inod_local, irank_home)
!
      use MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind=kint_gl), intent(in) :: istack_internod(0:nprocs)
      integer(kind=kint_gl), intent(in) :: istack_interele(0:nprocs)
      integer(kind=kint), intent(in) :: nnod_local
      integer(kind=kint), intent(in) :: inod_local(nnod_local)
      integer(kind=kint), intent(in) :: irank_home(nnod_local)
!
      integer(kind = kint) :: k1, inod, irank
      integer(kind = kint) :: i, led, ilength
      integer(kind = kint_gl) :: ie_tmp(0:nnod_4_ele)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = len_multi_int_textline(nnod_4_ele+1)
!
      led = izero
      if(nele .gt. 0) then
        led = ilength * nele
      end if
!
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nele .gt. 0) then
        do i = 1, nele
          ie_tmp(0) = id_global(i) + istack_interele(my_rank)
!
          do k1 = 1, nnod_4_ele
            inod = ie(i,k1)
            irank = irank_home(inod)
            ie_tmp(k1) = inod_local(inod) + istack_internod(irank)
          end do
!
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        multi_int8_textline((nnod_4_ele+1), ie_tmp))
        end do
      end if
!
      end subroutine mpi_write_merged_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_merged_grp(nshift8, IO_param, group_IO)
!
      use MPI_ascii_data_IO
      use MPI_domain_data_IO
      use data_IO_to_textline
!
      integer(kind=kint_gl), intent(in) :: nshift8
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(group_data), intent(inout) :: group_IO
!
      integer(kind = kint) :: i, ist, num
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
      call MPI_allREDUCE(num_item_l, istack_g(1), group_IO%num_grp,     &
     &    CALYPSO_GLOBAL_INT, MPI_SUM, CALYPSO_COMM, ierr_MPI)
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
          call mpi_write_charahead(IO_param, ione, chara_dat)
        else
          ist = group_IO%istack_grp(i-1) + 1
          num = group_IO%istack_grp(i) - group_IO%istack_grp(i-1)
          call mpi_write_grp_item                                       &
     &       (IO_param, ieight, num, group_IO%item_grp(ist), nshift8)
        end if
      end do
      call deallocate_grp_type(group_IO)
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
!
      integer(kind=kint_gl), intent(in) :: nshift8_ele
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint) :: i, num
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
      call MPI_allREDUCE(num_item_l, istack_g(1), surf_grp_IO%num_grp,  &
     &    CALYPSO_GLOBAL_INT, MPI_SUM, CALYPSO_COMM, ierr_MPI)
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
          call mpi_write_charahead(IO_param, ione, chara_dat)
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
      subroutine mpi_write_grp_item                                     &
     &         (IO_param, ncolumn, num, int_dat, nshift8)
!
      use MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: nshift8
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint_gl) :: int_tmp(num)
!
!
!$omp parallel workshare
       int_tmp(1:num) = int_dat(1:num) + nshift8
!$omp end parallel workshare
      call mpi_write_merged_group_item(IO_param, ncolumn, num, int_tmp)
!
      end subroutine mpi_write_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_surf_grp_item(IO_param, ncolumn,             &
     &          ntot, ist, num, int_dat, nshift8_ele)
!
      use MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: nshift8_ele
      integer(kind=kint), intent(in) :: ntot, ist, num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(2,ntot)
!
      integer(kind = kint_gl) :: int_tmp(num)
!
!
!$omp parallel workshare
       int_tmp(1:num) = int_dat(1,ist+1:ist+num) + nshift8_ele
!$omp end parallel workshare
      call mpi_write_merged_group_item(IO_param, ncolumn, num, int_tmp)
!
!
!$omp parallel workshare
       int_tmp(1:num) = int_dat(2,ist+1:ist+num)
!$omp end parallel workshare
      call mpi_write_merged_group_item(IO_param, ncolumn, num, int_tmp)
!
      end subroutine mpi_write_surf_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_merged_group_item                            &
     &         (IO_param, ncolumn, num, int_dat)
!
      use m_calypso_mpi_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint_gl), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, loop, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      led = izero
      if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_int_textline(ncolumn) * loop                    &
     &       + len_multi_int_textline(nrest)
      end if
!
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl + IO_param%istack_merged(my_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(nprocs)
!
      if(num .gt. 0) then
        do i = 0, (num-1)/ncolumn - 1
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &        len_multi_int_textline(ncolumn),                          &
     &        multi_int8_textline(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_int_textline(nrest),                              &
     &      multi_int8_textline(nrest, int_dat(num-nrest+1)))
      end if
!
      end subroutine mpi_write_merged_group_item
!
! -----------------------------------------------------------------------
!
      end module analyzer_merge_mesh
