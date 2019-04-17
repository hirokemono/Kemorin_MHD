!set_import_cube.f90
!     module set_import_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_import_data(c_size, nb_rng, loc_id, ipe, jpe)
!!      subroutine set_import_data_quad                                 &
!!     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe)
!!
!!      subroutine set_export_data(c_size, nb_rng, loc_id, ipe, jpe)
!!      subroutine set_export_data_quad                                 &
!!     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_import_cube
!
      use m_precision
!
      use t_neib_range_cube
      use t_size_of_cube
      use t_local_node_id_cube
      use m_comm_data_cube_kemo
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_import_data(c_size, nb_rng, loc_id, ipe, jpe)
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
      integer(kind = kint) :: inod
!
!
      neibpetot = 0
      inod   = 0
      call count_import_inside(c_size, nb_rng, neibpetot, inod)
      call count_import_peri_linear                                     &
     &   (c_size, nb_rng, ipe, jpe, neibpetot, inod)
      num_import = stack_import(neibpetot)
!
!                                     .... write nodes 
      neibpetot = 0
      inod = 0
      call set_import_inside(c_size, nb_rng, loc_id, neibpetot, inod)
      call set_import_peri                                              &
     &   (c_size, nb_rng, loc_id, ipe, jpe, neibpetot, inod)
!
      end subroutine set_import_data
!
! ----------------------------------------------------------------------
!
      subroutine set_import_data_quad                                   &
     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe)
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
      integer (kind = kint) :: inod
!
!
!                                     .... count nodes 
      inod = 0
      neibpetot = 0
      call count_import_inside_quad                                     &
     &   (c_size, nb_rng, kpe, neibpetot, inod)
      call count_import_peri_quad                                       &
     &   (c_size, nb_rng, ipe, jpe, kpe, neibpetot, inod)
!
      num_import = stack_import(neibpetot)
      write(*,*) ipe, jpe, kpe, 'num_import', num_import
!
!
!                                     .... write nodes 
      inod = 0
      neibpetot = 0
      call set_import_inside_quad                                       &
     &   (c_size, nb_rng, loc_id, kpe, neibpetot, inod)
      call set_import_peri_quad                                         &
     &   (c_size, nb_rng, loc_id, ipe, jpe, kpe, neibpetot, inod)
!
      write(*,*) ipe, jpe, kpe, 'import res.', inod
!
      end subroutine set_import_data_quad
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_export_data(c_size, nb_rng, loc_id, ipe, jpe)
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
      integer (kind = kint) :: inod
!
!
      neibpetot = 0
      inod   = 0
      call count_export_inside(c_size, nb_rng, neibpetot, inod)
      call count_export_peri_linear                                     &
     &   (c_size, nb_rng, ipe, jpe, neibpetot, inod)
      num_export = stack_export(neibpetot)
!
      inod = 0
      neibpetot = 0
      call set_export_inside(c_size, nb_rng, loc_id, neibpetot, inod)
      call set_export_peri                                              &
     &   (c_size, nb_rng, loc_id, ipe, jpe, neibpetot, inod)
!
      end subroutine set_export_data
!
! ----------------------------------------------------------------------
!
      subroutine set_export_data_quad                                   &
     &         (c_size, nb_rng, loc_id, ipe, jpe, kpe)
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
      integer (kind = kint) :: inod
!
! ***** set and write export nodes
!                                     .... count nodes 
      inod = 0
      neibpetot = 0
      call count_export_inside_quad                                     &
     &   (c_size, nb_rng, kpe, neibpetot, inod)
      call count_export_peri_quad                                       &
     &   (c_size, nb_rng, ipe, jpe, kpe, neibpetot, inod)

      num_export = stack_export(neibpetot)
!
      inod = 0
      neibpetot = 0
      call set_export_inside_quad                                       &
     &   (c_size, nb_rng, loc_id, kpe, neibpetot, inod)
      call set_export_peri_quad                                         &
     &   (c_size, nb_rng, loc_id, ipe, jpe, kpe, neibpetot, inod)
!
      end subroutine set_export_data_quad
!
! ----------------------------------------------------------------------
!
      end module set_import_cube
