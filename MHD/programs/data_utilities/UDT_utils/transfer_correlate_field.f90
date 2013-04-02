!transfer_correlate_field.f90
!      module transfer_correlate_field
!
      module transfer_correlate_field
!
!     Written by H. Matsui on Nov., 2009
!
      use m_precision
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_phys_data
!
      implicit none
!
      real(kind = kreal), allocatable :: d_nod_trans1(:,:)
      real(kind = kreal), allocatable :: d_nod_trans2(:,:)
!
      private :: d_nod_trans1
!
!      subroutine allocate_vec_transfer
!      subroutine deallocate_vec_transfer
!      subroutine transfer_corr_field_to_sph
!      subroutine transfer_corr_field_to_cyl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_vec_transfer
!
      use m_geometry_parameter
!
      allocate(d_nod_trans1(numnod,6))
      allocate(d_nod_trans2(numnod,6))
      d_nod_trans1 = 0.0d0
      d_nod_trans2 = 0.0d0
!
      end subroutine allocate_vec_transfer
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_vec_transfer
!
      deallocate(d_nod_trans1, d_nod_trans2)
!
      end subroutine deallocate_vec_transfer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine transfer_corr_field_to_sph
!
      use m_node_phys_data
      use cvt_vector_2_spheric_smp
      use cvt_tensor_2_spheric_smp
!
      integer(kind = kint) :: i_fld, ist, inod
!
!
        do i_fld = 1, num_nod_phys
          ist = istack_nod_component(i_fld-1) + 1
          if     (num_nod_component(i_fld) .eq. 3) then
!
!$omp parallel do
            do inod = 1, numnod
              d_nod_trans1(inod,1) = d_nod(inod,ist  )
              d_nod_trans1(inod,2) = d_nod(inod,ist+1)
              d_nod_trans1(inod,3) = d_nod(inod,ist+2)
              d_nod_trans2(inod,1) = d_nod_2nd(inod,ist  )
              d_nod_trans2(inod,2) = d_nod_2nd(inod,ist+1)
              d_nod_trans2(inod,3) = d_nod_2nd(inod,ist+2)
            end do
!$omp end parallel do
!
            call cvt_vector_2_sph_smp(np_smp, numnod, inod_smp_stack,   &
     &          d_nod_trans1(1,1), d_nod(1,ist), xx, radius,            &
     &          s_cylinder, a_radius, a_s_cylinder)
!
            call cvt_vector_2_sph_smp(np_smp, numnod, inod_smp_stack,   &
     &          d_nod_trans2(1,1), d_nod_trans1(1,1), xx, radius,       &
     &          s_cylinder, a_radius, a_s_cylinder)
!
!$omp parallel do
            do inod = 1, numnod
              d_nod_2nd(inod,ist  ) = d_nod_trans1(inod,1)
              d_nod_2nd(inod,ist+1) = d_nod_trans1(inod,2)
              d_nod_2nd(inod,ist+2) = d_nod_trans1(inod,3)
            end do
!$omp end parallel do
!
          else if(num_nod_component(i_fld) .eq. 6) then
!
!$omp parallel do
            do inod = 1, numnod
              d_nod_trans1(inod,1) = d_nod(inod,ist  )
              d_nod_trans1(inod,2) = d_nod(inod,ist+1)
              d_nod_trans1(inod,3) = d_nod(inod,ist+2)
              d_nod_trans1(inod,4) = d_nod(inod,ist+3)
              d_nod_trans1(inod,5) = d_nod(inod,ist+4)
              d_nod_trans1(inod,6) = d_nod(inod,ist+5)
              d_nod_trans2(inod,1) = d_nod_2nd(inod,ist  )
              d_nod_trans2(inod,2) = d_nod_2nd(inod,ist+1)
              d_nod_trans2(inod,3) = d_nod_2nd(inod,ist+2)
              d_nod_trans2(inod,4) = d_nod_2nd(inod,ist+3)
              d_nod_trans2(inod,5) = d_nod_2nd(inod,ist+4)
              d_nod_trans2(inod,6) = d_nod_2nd(inod,ist+5)
            end do
!$omp end parallel do
!
            call cal_sph_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &          d_nod_trans1(1,1), d_nod(1,ist), xx, radius,            &
     &          s_cylinder, a_radius, a_s_cylinder)
!
            call cal_sph_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &          d_nod_trans2(1,1), d_nod_trans1(1,1), xx, radius,       &
     &          s_cylinder, a_radius, a_s_cylinder)
!
!$omp parallel do
            do inod = 1, numnod
              d_nod_2nd(inod,ist  ) = d_nod_trans1(inod,1)
              d_nod_2nd(inod,ist+1) = d_nod_trans1(inod,2)
              d_nod_2nd(inod,ist+2) = d_nod_trans1(inod,3)
              d_nod_2nd(inod,ist+3) = d_nod_trans1(inod,4)
              d_nod_2nd(inod,ist+4) = d_nod_trans1(inod,5)
              d_nod_2nd(inod,ist+5) = d_nod_trans1(inod,6)
            end do
!$omp end parallel do
!
          end if
        end do
!
      end subroutine transfer_corr_field_to_sph
!
!  ---------------------------------------------------------------------
!
      subroutine transfer_corr_field_to_cyl
!
      use m_node_phys_data
      use cvt_vector_2_cylinder_smp
      use cvt_tensor_2_cylinder_smp
!
      integer(kind = kint) :: i_fld, ist, inod
!
!
        do i_fld = 1, num_nod_phys
          ist = istack_nod_component(i_fld-1) + 1
          if     (num_nod_component(i_fld) .eq. 3) then
!
!$omp parallel do
            do inod = 1, numnod
              d_nod_trans1(inod,1) = d_nod(inod,ist  )
              d_nod_trans1(inod,2) = d_nod(inod,ist+1)
              d_nod_trans1(inod,3) = d_nod(inod,ist+2)
              d_nod_trans2(inod,1) = d_nod_2nd(inod,ist  )
              d_nod_trans2(inod,2) = d_nod_2nd(inod,ist+1)
              d_nod_trans2(inod,3) = d_nod_2nd(inod,ist+2)
            end do
!$omp end parallel do
!
            call cvt_vector_2_cyl_smp(np_smp, numnod,                   &
     &          inod_smp_stack, d_nod_trans1(1,1), d_nod(1,ist),        &
     &          xx, s_cylinder, a_s_cylinder)
!
            call cvt_vector_2_cyl_smp(np_smp, numnod,                   &
     &          inod_smp_stack, d_nod_trans2(1,1), d_nod_trans1(1,1),   &
     &          xx, s_cylinder, a_s_cylinder)
!
!$omp parallel do
            do inod = 1, numnod
              d_nod_2nd(inod,ist  ) = d_nod_trans1(inod,1)
              d_nod_2nd(inod,ist+1) = d_nod_trans1(inod,2)
              d_nod_2nd(inod,ist+2) = d_nod_trans1(inod,3)
            end do
!$omp end parallel do
!
          else if(num_nod_component(i_fld) .eq. 6) then
!
!$omp parallel do
            do inod = 1, numnod
              d_nod_trans1(inod,1) = d_nod(inod,ist  )
              d_nod_trans1(inod,2) = d_nod(inod,ist+1)
              d_nod_trans1(inod,3) = d_nod(inod,ist+2)
              d_nod_trans1(inod,4) = d_nod(inod,ist+3)
              d_nod_trans1(inod,5) = d_nod(inod,ist+4)
              d_nod_trans1(inod,6) = d_nod(inod,ist+5)
              d_nod_trans2(inod,1) = d_nod_2nd(inod,ist  )
              d_nod_trans2(inod,2) = d_nod_2nd(inod,ist+1)
              d_nod_trans2(inod,3) = d_nod_2nd(inod,ist+2)
              d_nod_trans2(inod,4) = d_nod_2nd(inod,ist+3)
              d_nod_trans2(inod,5) = d_nod_2nd(inod,ist+4)
              d_nod_trans2(inod,6) = d_nod_2nd(inod,ist+5)
            end do
!$omp end parallel do
!
            call cal_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &          d_nod_trans1(1,1), d_nod(1,ist), xx,                    &
     &          s_cylinder, a_s_cylinder)
!
            call cal_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &          d_nod_trans2(1,1), d_nod_trans1(1,1), xx,               &
     &          s_cylinder, a_s_cylinder)
!
!$omp parallel do
            do inod = 1, numnod
              d_nod_2nd(inod,ist  ) = d_nod_trans1(inod,1)
              d_nod_2nd(inod,ist+1) = d_nod_trans1(inod,2)
              d_nod_2nd(inod,ist+2) = d_nod_trans1(inod,3)
              d_nod_2nd(inod,ist+3) = d_nod_trans1(inod,4)
              d_nod_2nd(inod,ist+4) = d_nod_trans1(inod,5)
              d_nod_2nd(inod,ist+5) = d_nod_trans1(inod,6)
            end do
!$omp end parallel do
!
          end if
        end do
!
      end subroutine transfer_corr_field_to_cyl
!
!  ---------------------------------------------------------------------
!
      end module transfer_correlate_field
