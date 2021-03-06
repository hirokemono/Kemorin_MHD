!set_import_cube.f90
!     module set_import_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_neigbouring_plane                                &
!!     &         (c_size, nb_rng, pe_id, ipe, jpe, comm)
!!
!!      subroutine set_import_data                                      &
!!     &         (c_size, nb_rng, loc_id, ipe, jpe, comm)
!!      subroutine set_import_data_quad                                 &
!!     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe, comm)
!!
!!      subroutine set_export_data                                      &
!!     &         (c_size, nb_rng, loc_id, ipe, jpe, comm)
!!      subroutine set_export_data_quad                                 &
!!     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe, comm)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(communication_table), intent(inout) :: comm
!
      module set_import_cube
!
      use m_precision
!
      use t_neib_range_cube
      use t_size_of_cube
      use t_local_node_id_cube
      use t_comm_table
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_neigbouring_plane                                  &
     &         (c_size, nb_rng, pe_id, ipe, jpe, comm)
!
      use set_neib_pe_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      integer(kind = kint), intent(in) :: pe_id, ipe, jpe
!
      type(communication_table), intent(inout) :: comm
!
      integer (kind = kint) :: icou_pe
!
!
      call count_neighboring_pes(nb_rng, comm%num_neib)
      call count_neighboring_pes_peri                                   &
     &   (nb_rng, c_size%ndx, c_size%ndy, ipe, jpe, comm%num_neib)
!
      call alloc_comm_table_num(comm)
!
!      inside cube
!
      icou_pe = 0
      call set_neighboring_pes                                          &
     &         (nb_rng, c_size%ndx, c_size%ndy, pe_id,                  &
     &          comm%num_neib, comm%id_neib, icou_pe)
!
!      neiboring information for periodical boundaries
!
      call set_neighboring_pes_peri                                     &
     &         (nb_rng, c_size%ndx, c_size%ndy, pe_id, ipe, jpe,        &
     &          comm%num_neib, comm%id_neib, icou_pe)
      comm%id_neib(1:comm%num_neib) = comm%id_neib(1:comm%num_neib) - 1
!
      end subroutine set_neigbouring_plane
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_import_data                                        &
     &         (c_size, nb_rng, loc_id, ipe, jpe, comm)
!
      use count_import_inside_cube
      use set_import_inside_cube
      use count_import_peri
      use set_import_peri_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer(kind = kint), intent(in) :: ipe, jpe
!
      type(communication_table), intent(inout) :: comm
!
      integer (kind = kint) :: inod, icou_pe
!
!
      icou_pe = 0
      inod   = 0
      call count_import_inside(c_size, nb_rng,                          &
     &    comm%num_neib, comm%istack_import, icou_pe, inod)
      call count_import_peri_linear(c_size, nb_rng, ipe, jpe,           &
     &    comm%num_neib, comm%istack_import, icou_pe, inod)
!
      comm%ntot_import = comm%istack_import(comm%num_neib)
      call alloc_import_item(comm)
!
      icou_pe = 0
      inod = 0
      call set_import_inside                                            &
     &   (c_size, nb_rng, loc_id, comm, icou_pe, inod)
      call set_import_peri                                              &
     &   (c_size, nb_rng, loc_id, ipe, jpe, comm, icou_pe, inod)
!
      end subroutine set_import_data
!
! ----------------------------------------------------------------------
!
      subroutine set_import_data_quad                                   &
     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe, comm)
!
      use count_import_inside_cube
      use set_import_inside_cube
      use count_import_peri
      use set_import_peri_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      type(communication_table), intent(inout) :: comm
!
      integer (kind = kint) :: inod, icou_pe
!
!
!                                     .... count nodes 
      inod = 0
      icou_pe = 0
      call count_import_inside_quad(c_size, nb_rng, kpe,                &
     &    comm%num_neib, comm%istack_import, icou_pe, inod)
      call count_import_peri_quad(c_size, nb_rng, ipe, jpe, kpe,        &
     &    comm%num_neib, comm%istack_import, icou_pe, inod)
!
      comm%ntot_import = comm%istack_import(comm%num_neib)
      call alloc_import_item(comm)
!
!                                     .... write nodes 
      inod = 0
      icou_pe = 0
      call set_import_inside_quad                                       &
     &   (c_size, nb_rng, loc_id, kpe, comm, icou_pe, inod)
      call set_import_peri_quad                                         &
     &   (c_size, nb_rng, loc_id, ipe, jpe, kpe, comm, icou_pe, inod)
!
      write(*,*) ipe, jpe, kpe, 'import res.', inod
!
      end subroutine set_import_data_quad
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_export_data                                        &
     &         (c_size, nb_rng, loc_id, ipe, jpe, comm)
!
      use count_export_inside_cube
      use set_export_inside_cube
      use count_export_peri
      use set_export_peri_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: ipe, jpe
!
      type(communication_table), intent(inout) :: comm
!
      integer (kind = kint) :: inod, icou_pe
!
!
      icou_pe = 0
      inod   = 0
      call count_export_inside(c_size, nb_rng,                          &
     &    comm%num_neib, comm%istack_export, icou_pe, inod)
      call count_export_peri_linear(c_size, nb_rng, ipe, jpe,           &
     &    comm%num_neib, comm%istack_export, icou_pe, inod)
!
      comm%ntot_export = comm%istack_export(comm%num_neib)
      call alloc_export_item(comm)
!
      inod = 0
      icou_pe = 0
      call set_export_inside                                            &
     &   (c_size, nb_rng, loc_id, comm, icou_pe, inod)
      call set_export_peri                                              &
     &   (c_size, nb_rng, loc_id, ipe, jpe, comm, icou_pe, inod)
!
      end subroutine set_export_data
!
! ----------------------------------------------------------------------
!
      subroutine set_export_data_quad                                   &
     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe, comm)
!
      use count_export_inside_cube
      use set_export_inside_cube
      use count_export_peri
      use set_export_peri_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      type(communication_table), intent(inout) :: comm
!
      integer (kind = kint) :: inod, icou_pe
!
! ***** set and write export nodes
!                                     .... count nodes 
      inod = 0
      icou_pe = 0
      call count_export_inside_quad(c_size, nb_rng, kpe,                &
     &    comm%num_neib, comm%istack_export, icou_pe, inod)
      call count_export_peri_quad(c_size, nb_rng, ipe, jpe, kpe,        &
     &    comm%num_neib, comm%istack_export, icou_pe, inod)
!
      comm%ntot_export = comm%istack_export(comm%num_neib)
      call alloc_export_item(comm)
!
      inod = 0
      icou_pe = 0
      call set_export_inside_quad                                       &
     &   (c_size, nb_rng, loc_id, kpe, comm, icou_pe, inod)
      call set_export_peri_quad                                         &
     &   (c_size, nb_rng, loc_id, ipe, jpe, kpe, comm, icou_pe, inod)
!
      end subroutine set_export_data_quad
!
! ----------------------------------------------------------------------
!
      end module set_import_cube
