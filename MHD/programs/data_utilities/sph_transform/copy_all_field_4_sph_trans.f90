!copy_all_field_4_sph_trans.f90
!     module copy_all_field_4_sph_trans
!
!      subroutine allocate_wk_nod_4_sph_trans
!      subroutine deallocate_wk_nod_4_sph_trans
!
!      subroutine set_sph_scalar_to_sph_trans
!      subroutine set_sph_scalar_from_sph_trans
!
!      subroutine set_sph_vect_to_sph_trans
!      subroutine set_xyz_vect_from_sph_trans
!
!      subroutine set_sph_tensor_to_sph_trans
!      subroutine set_sph_tensor_from_sph_trans
!
!      Written by H. Matsui on Feb., 2008
!
      module copy_all_field_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
      use m_sph_spectr_data
      use m_work_4_sph_trans
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_sph_scalar_to_sph_trans
!
      use copy_1st_nodal_4_sph_trans
!
      integer(kind = kint) :: j, j0
      integer(kind = kint) :: i, i_field, itrans
!
!
      do j = 1, num_scalar_rtp
        j0 = istart_scalar_rtp + j - 1
        itrans = j + 3*num_vector_rtp
!
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
            i_field = istack_nod_component(i- 1) + 1
            call copy_1st_scl_to_sph_trans(itrans, i_field)
            exit
          end if
        end do
      end do
!
      end subroutine set_sph_scalar_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine set_sph_scalar_from_sph_trans
!
      use copy_1st_nodal_4_sph_trans
!
      integer(kind = kint) :: j, j0
      integer(kind = kint) :: i, i_field, itrans
!
!
      do j = 1, num_scalar_rtp
        j0 = istart_scalar_rtp + j - 1
        itrans = j + 3*num_vector_rtp
!
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
            i_field = istack_nod_component(i- 1) + 1
            call copy_1st_scl_from_sph_trans(itrans, i_field)
            exit
          end if
        end do
      end do
!
      end subroutine set_sph_scalar_from_sph_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_sph_vect_to_sph_trans
!
      use copy_1st_nodal_4_sph_trans
!
      integer(kind = kint) :: j, j0
      integer(kind = kint) :: i, i_field, itrans
!
!
      do j = 1, num_vector_rtp
        j0 = istart_vector_rtp + j - 1
        itrans = 3*j - 2
!
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
            i_field = istack_nod_component(i- 1) + 1
            call copy_1st_vec_to_sph_trans(itrans, i_field)
            exit
          end if
        end do
      end do
!
      end subroutine set_sph_vect_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine set_xyz_vect_from_sph_trans
!
      use copy_1st_nodal_4_sph_trans
!
      integer(kind = kint) :: j, j0
      integer(kind = kint) :: i, i_field, itrans
!
!
      do j = 1, num_vector_rtp
        j0 = istart_vector_rtp + j - 1
        itrans = 3*j - 2
!
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
            i_field = istack_nod_component(i- 1) + 1
            call copy_1st_vec_from_trans_wpole(ncomp_sph_trans,         &
     &          itrans, i_field)
            exit
          end if
        end do
      end do
!
      end subroutine set_xyz_vect_from_sph_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_sph_tensor_to_sph_trans
!
      use copy_1st_nodal_4_sph_trans
!
      integer(kind = kint) :: j, j0
      integer(kind = kint) :: i, i_field, itrans
!
!
      do j = 1, num_tensor_rtp
        j0 = istart_tensor_rtp + j - 1
        itrans = 1 + 6*(j-1) + num_scalar_rtp + 3*num_vector_rtp
!
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
            i_field = istack_nod_component(i- 1) + 1
            call copy_1st_tsr_to_sph_trans(itrans, i_field)
            exit
          end if
        end do
      end do
!
      end subroutine set_sph_tensor_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine set_sph_tensor_from_sph_trans
!
      use copy_1st_nodal_4_sph_trans
!
      integer(kind = kint) :: j, j0
      integer(kind = kint) :: i, i_field, itrans
!
!
      do j = 1, num_tensor_rtp
        j0 = istart_tensor_rtp + j - 1
        itrans = 1 + 6*(j-1) + num_scalar_rtp + 3*num_vector_rtp
!
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
            i_field = istack_nod_component(i- 1) + 1
            call copy_1st_tsr_from_trans_wpole(ncomp_sph_trans, itrans, &
     &          i_field)
            exit
          end if
        end do
      end do
!
      end subroutine set_sph_tensor_from_sph_trans
!
! -------------------------------------------------------------------
!
      end module copy_all_field_4_sph_trans
