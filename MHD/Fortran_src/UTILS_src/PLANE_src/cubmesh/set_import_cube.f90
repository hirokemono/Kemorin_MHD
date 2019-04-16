!set_import_cube.f90
!     module set_import_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_import_data(nb_rng, ipe, jpe)
!!      subroutine set_import_data_quad(nb_rng, ipe, jpe, kpe)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_import_cube
!
      use m_precision
!
      use t_neib_range_cube
      use m_size_of_cube
      use m_comm_data_cube_kemo
      use count_import_inside_cube
      use set_import_inside_cube
      use count_import_peri
      use set_import_peri_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_import_data(nb_rng, ipe, jpe)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer(kind = kint), intent(in) :: ipe, jpe
!
      integer(kind = kint) :: inod
!
!
      neibpetot = 0
      inod   = 0
      call count_import_inside(c_size1, nb_rng, neibpetot, inod)
      call count_import_peri_linear                                     &
     &   (c_size1, nb_rng, ipe, jpe, neibpetot, inod)
      num_import = stack_import(neibpetot)
!
!                                     .... write nodes 
      neibpetot = 0
      inod = 0
      call set_import_inside(c_size1, nb_rng, neibpetot, inod)
      call set_import_peri(c_size1, nb_rng, ipe, jpe, neibpetot, inod)
!
      end subroutine set_import_data
!
! ----------------------------------------------------------------------
!
      subroutine set_import_data_quad(nb_rng, ipe, jpe, kpe)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      integer (kind = kint) :: inod
!
!
!                                     .... count nodes 
      inod = 0
      neibpetot = 0
      call count_import_inside_quad                                     &
     &   (c_size1, nb_rng, kpe, neibpetot, inod)
      call count_import_peri_quad                                       &
     &   (c_size1, nb_rng, ipe, jpe, kpe, neibpetot, inod)
!
      num_import = stack_import(neibpetot)
      write(*,*) ipe, jpe, kpe, 'num_import', num_import
!
!
!                                     .... write nodes 
      inod = 0
      neibpetot = 0
      call set_import_inside_quad                                       &
     &   (c_size1, nb_rng, kpe, neibpetot, inod)
      call set_import_peri_quad                                         &
     &   (c_size1, nb_rng, ipe, jpe, kpe, neibpetot, inod)
!
      write(*,*) ipe, jpe, kpe, 'import res.', inod
!
      end subroutine set_import_data_quad
!
! ----------------------------------------------------------------------
!
      end module set_import_cube
