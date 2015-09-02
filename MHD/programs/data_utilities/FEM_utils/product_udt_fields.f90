!
!      module product_udt_fields
!
!      Written by H. Matsui on Dec., 2007
!
!      subroutine allocate_product_data(numnod)
!      subroutine allocate_product_result
!      subroutine deallocate_product_data
!
!      subroutine set_field_id_4_product
!
!      subroutine set_data_for_product(numnod, istep_ucd)
!      subroutine cal_products_of_fields
!
      module product_udt_fields
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      real(kind = kreal), allocatable :: d_prod1(:,:)
      real(kind = kreal), allocatable :: d_prod2(:,:)
!
      private :: d_prod1, d_prod2
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_product_data(numnod)
!
      use m_ctl_params_4_prod_udt
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate(d_prod1(numnod,ncomp_4_product1))
      allocate(d_prod2(numnod,ncomp_4_product2))
      d_prod1 = 0.0d0
      d_prod2 = 0.0d0
!
      end subroutine allocate_product_data
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_product_data
!
!
      deallocate(d_prod1, d_prod2)
!
      end subroutine deallocate_product_data
!
!-----------------------------------------------------------------------
!
      subroutine allocate_product_result
!
      use m_ctl_params_4_prod_udt
      use m_node_phys_data
!
!
      num_nod_phys =     ione
      num_nod_phys_vis = ione
      call allocate_phys_name
!
      num_nod_component(1) =    ncomp_4_result
      istack_nod_component(1) = ncomp_4_result
      iflag_nod_fld_monitor(1) = 1
      phys_nod_name(1) = result_field_name
!
      num_tot_nod_phys =     ncomp_4_result
      num_tot_nod_phys_vis = ncomp_4_result
      call allocate_data_arrays
!
      end subroutine allocate_product_result
!
!-----------------------------------------------------------------------
!
      subroutine set_field_id_4_product
!
      use calypso_mpi
      use m_error_IDs
      use m_ctl_params_4_prod_udt
      use m_ucd_data
      use m_ucd_input_data
      use m_t_step_parameter
      use ucd_IO_select
!
      integer(kind = kint) :: istep_ucd
!
!
      write(*,*) 'i_step_init', i_step_init, i_step_output_ucd
      istep_ucd = i_step_init / i_step_output_ucd
      call find_field_id_in_read_ucd(my_rank, istep_ucd,                &
     &   ifmt_result_udt_file, prod_udt_file1_head,                     &
     &product_field_1_name, i_field_product1, ncomp_4_product1)
!
      call find_field_id_in_read_ucd(my_rank, istep_ucd,                &
     &   ifmt_result_udt_file, prod_udt_file2_head,                     &
     &   product_field_2_name, i_field_product2, ncomp_4_product2)
!
      if( (i_field_product1*i_field_product2) .eq. 0) then
        call calypso_MPI_abort(ierr_fld,'Field does not excist')
      end if
!
      if(ncomp_4_product1.eq.1) then
        if(ncomp_4_product2 .eq. 1) then
          ncomp_4_result = 1
        else if(ncomp_4_product2 .eq.3) then
          ncomp_4_result = 3
        else if(ncomp_4_product2 .eq.6) then
          ncomp_4_result = 6
        end if
      else if(ncomp_4_product1.eq.3) then
        if(ncomp_4_product2 .eq. 1) then
          ncomp_4_result = 3
        else if(ncomp_4_product2 .eq.3) then
          if(iflag_product_type .eq. 2) then
            ncomp_4_result = 3
          else
            ncomp_4_result = 1
          end if
        else if(ncomp_4_product2 .eq.6) then
            ncomp_4_result = 3
        end if
      else if(ncomp_4_product1.eq.6) then
        if(ncomp_4_product2 .eq. 1) then
          ncomp_4_result = 6
        else if(ncomp_4_product2 .eq.3) then
          ncomp_4_result = 3
        else if(ncomp_4_product2 .eq.6) then
          ncomp_4_result = 6
        end if
      end if
!
      if(my_rank .eq. 0) then
        write(*,*) trim(prod_udt_file1_head), ': ',                     &
     &             trim(product_field_1_name), ': ',                    &
     &             i_field_product1, ncomp_4_product1
        write(*,*) trim(prod_udt_file2_head), ': ',                     &
     &             trim(product_field_2_name), ': ',                    &
     &             i_field_product2, ncomp_4_product2
      end if
!
      end subroutine set_field_id_4_product
!
!-----------------------------------------------------------------------
!
      subroutine set_data_for_product(numnod, istep_ucd)
!
      use calypso_mpi
      use m_ctl_params_4_prod_udt
      use m_ucd_data
      use m_ucd_input_data
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: istep_ucd
!
!
      call set_one_field_by_read_ucd_once(my_rank, istep_ucd,           &
     &    ifmt_result_udt_file, prod_udt_file1_head,                    &
     &    i_field_product1, ncomp_4_product1, numnod, d_prod1)
!
      call set_one_field_by_read_ucd_once(my_rank, istep_ucd,           &
     &    ifmt_result_udt_file, prod_udt_file2_head,                    &
     &    i_field_product2, ncomp_4_product2, numnod, d_prod2)
!
      end subroutine set_data_for_product
!
!-----------------------------------------------------------------------
!
      subroutine cal_rev_of_2nd_field
!
      use m_geometry_data
      use m_ctl_params_4_prod_udt
!
      integer(kind = kint) :: nd, inod
!
!
       do nd = 1, ncomp_4_product2
         do inod = 1, node1%numnod
           if(d_prod2(inod,nd) .eq. zero) then
             d_prod2(inod,nd) = zero
           else
             d_prod2(inod,nd) = one / d_prod2(inod,nd)
           end if
         end do
       end do
!
!
      end subroutine cal_rev_of_2nd_field
!
!-----------------------------------------------------------------------
!
      subroutine cal_products_of_fields
!
      use m_geometry_data
      use m_ctl_params_4_prod_udt
      use m_node_phys_data
      use cal_products_smp
      use nod_phys_send_recv
!
!
      if(ncomp_4_product1.eq.1) then
        if(ncomp_4_product2 .eq. 1) then
!$omp parallel
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,1),  d_nod(1,1))
!$omp end parallel
        else if(ncomp_4_product2 .eq.3) then
!$omp parallel
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,1),  d_nod(1,1))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,2),  d_nod(1,2))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,3),  d_nod(1,3))
!$omp end parallel
        else if(ncomp_4_product2 .eq.6) then
!$omp parallel
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,1),  d_nod(1,1))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,2),  d_nod(1,2))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,3),  d_nod(1,3))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,4),  d_nod(1,4))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,5),  d_nod(1,5))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,6),  d_nod(1,6))
!$omp end parallel
        end if
      else if(ncomp_4_product1.eq.3) then
        if(ncomp_4_product2 .eq. 1) then
!$omp parallel
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,1),  d_nod(1,1))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,2), d_prod2(1,1),  d_nod(1,2))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,3), d_prod2(1,1),  d_nod(1,3))
!$omp end parallel
        else if(ncomp_4_product2 .eq.3) then
          if(iflag_product_type .eq. 2) then
!$omp parallel
           call cal_cross_prod_no_coef_smp                              &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,1),  d_nod(1,1))
!$omp end parallel
          else
!$omp parallel
           call cal_dot_prod_no_coef_smp                                &
     &        (np_smp, node1%numnod,  node1%istack_nod_smp,             &
     &         d_prod1(1,1), d_prod2(1,1),  d_nod(1,1))
!$omp end parallel
          end if
        else if(ncomp_4_product2 .eq.6) then
!$omp parallel
           call cal_tensor_vec_prod_no_coef_smp                         &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod2(1,1), d_prod1(1,1),  d_nod(1,1))
!$omp end parallel
        end if
      else if(ncomp_4_product1.eq.6) then
        if(ncomp_4_product2 .eq. 1) then
!$omp parallel
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,1),  d_nod(1,1))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,2), d_prod2(1,1),  d_nod(1,2))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,3), d_prod2(1,1),  d_nod(1,3))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,4), d_prod2(1,1),  d_nod(1,4))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,5), d_prod2(1,1),  d_nod(1,5))
           call cal_scalar_prod_no_coef_smp                             &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,6), d_prod2(1,1),  d_nod(1,6))
!$omp end parallel
        else if(ncomp_4_product2 .eq.3) then
!$omp parallel
           call cal_tensor_vec_prod_no_coef_smp                         &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,1),  d_nod(1,1))
!$omp end parallel
        else if(ncomp_4_product2 .eq.6) then
!$omp parallel
           call cal_tensor_vec_prod_no_coef_smp                         &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,1),  d_nod(1,1))
           call cal_tensor_vec_prod_no_coef_smp                         &
     &        (np_smp, node1%numnod, node1%istack_nod_smp,              &
     &         d_prod1(1,1), d_prod2(1,4),  d_nod(1,4))
!$omp end parallel
        end if
      end if
!
!
      if(ncomp_4_result .eq. ione) then
        call scalar_send_recv(ione, ione, d_nod)
      else if(ncomp_4_result .eq. ithree) then
        call vector_send_recv(ithree, ione, d_nod)
      else if(ncomp_4_result .eq. isix) then
        call sym_tensor_send_recv(isix, ione, d_nod)
      end if
!
!      write(50+my_rank,*) 'd_nod'
!      do i = 1, node1%numnod
!        write(50+my_rank,*) i, inod_global(i),   &
!     &          d_prod1(i,1:ncomp_4_product1),   &
!     &          d_prod2(i,1:ncomp_4_product2),   &
!     &         d_nod(i,1:num_tot_nod_phys_vis)
!      end do
!
      end subroutine cal_products_of_fields
!
!-----------------------------------------------------------------------
!
      end module product_udt_fields
