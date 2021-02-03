!>@file   compare_psf_files.f90
!!@brief  module compare_psf_files
!!
!!@date  Programmed by H.Matsui in Jan., 2021
!
!>@brief control data for cross sections
!
      program compare_psf_files
!
      use m_precision
      use t_ctl_data_psf_compares
!
      use t_file_IO_parameter
      use t_time_data
      use t_control_params_4_psf
!
      implicit none
!
      type(psf_compare_controls), save :: psf_cmp_list1
!
      integer(kind = kint) :: num_psf_list
      type(field_IO_params), allocatable, save:: psf1_file_param(:)
      type(field_IO_params), allocatable, save :: psf2_file_param(:)
      integer(kind = kint), allocatable :: istep_psf(:)
!
      integer(kind = kint) :: i
!
      call read_ctl_file_psf_compares(0, psf_cmp_list1)
!
      num_psf_list = psf_cmp_list1%num_psf_cmp
      allocate(psf1_file_param(num_psf_list))
      allocate(psf2_file_param(num_psf_list))
      allocate(istep_psf(num_psf_list))
!
      do i = 1, num_psf_list
        call set_control_for_psf_compare(psf_cmp_list1%psf_cmp_ctls(i), &
     &      istep_psf(i), psf1_file_param(i), psf2_file_param(i))
      end do
!
      call dealloc_psf_compares_ctl(psf_cmp_list1)
!
      do i = 1, num_psf_list
        call compare_psf_data                                           &
     &     (istep_psf(i), psf1_file_param(i), psf2_file_param(i))
      end do
!
      deallocate(psf1_file_param)
      deallocate(psf2_file_param)
      deallocate(istep_psf)
!
      end program compare_psf_files
!
!  --------------------------------------------------------------------
!
      subroutine set_control_for_psf_compare(psf_cmp_ctls,              &
     &          istep_psf, psf1_file_param, psf2_file_param)
!
      use t_ctl_data_psf_compare
      use t_file_IO_parameter
      use t_control_params_4_psf
!
      type(psf_compare_control), intent(in) :: psf_cmp_ctls
      type(field_IO_params), intent(inout):: psf1_file_param
      type(field_IO_params), intent(inout) :: psf2_file_param
      integer(kind = kint), intent(inout) :: istep_psf
!
      character(len=kchara), parameter :: default_psf_prefix = 'psf'
!
      call set_read_psf_file_ctl(default_psf_prefix,                    &
     &    psf_cmp_ctls%first_psf%file_prefix_ctl,                       &
     &    psf_cmp_ctls%first_psf%file_format_ctl, psf1_file_param)
      call set_read_psf_file_ctl(default_psf_prefix,                    &
     &    psf_cmp_ctls%second_psf%file_prefix_ctl,                      &
     &    psf_cmp_ctls%second_psf%file_format_ctl, psf2_file_param)
      istep_psf = psf_cmp_ctls%i_step_surface_ctl%intvalue
!
      end subroutine set_control_for_psf_compare
!
!  --------------------------------------------------------------------
!
      subroutine compare_psf_data                                       &
     &         (istep_psf, psf1_file_param, psf2_file_param)
!
      use m_precision
      use m_machine_parameter
!
      use t_psf_results
      use t_ucd_data
!
      implicit none
!
      integer(kind = kint), intent(in) :: istep_psf
      type(field_IO_params), intent(in) :: psf1_file_param
      type(field_IO_params), intent(in) :: psf2_file_param
!
      integer(kind = kint) :: compare_field_vector
      external :: compare_field_vector
!
      type(psf_results), save :: psf_1
      type(psf_results), save :: psf_2
      type(time_data), save :: t_IO_u
      type(ucd_data), save:: psf_ucd
!
      integer(kind = kint) :: compare_node_position, compare_ele_connect
      integer(kind = kint) :: compare_field_data
      external :: compare_node_position, compare_ele_connect
      external :: compare_field_data
!
      integer(kind = kint) :: iflag
!
!
      call load_psf_data_to_link_IO                                   &
     &   (istep_psf, psf1_file_param, t_IO_u, psf_1, psf_ucd)
      call load_psf_data_to_link_IO                                   &
     &   (istep_psf, psf2_file_param, t_IO_u, psf_2, psf_ucd)
!
      iflag = compare_node_position(psf_1%psf_nod, psf_2%psf_nod)
      iflag = iflag                                                   &
     &       + compare_ele_connect(psf_1%psf_ele, psf_2%psf_ele)
      iflag = iflag                                                   &
     &       + compare_field_data(psf_1%psf_phys, psf_2%psf_phys)
!
      if(iflag .eq. 0) then
        write(*,*) trim(psf1_file_param%file_prefix), ' and ',        &
     &             trim(psf2_file_param%file_prefix),                 &
     &            ' have same data.'
      else
        write(*,*) trim(psf1_file_param%file_prefix), ' and ',        &
     &             trim(psf2_file_param%file_prefix),                 &
     &            ' is different.'
      end if
!
      call dealloc_psf_results(psf_1)
      call dealloc_psf_results(psf_2)
!
      end subroutine compare_psf_data
!
!  --------------------------------------------------------------------
!
      integer(kind = kint) function compare_node_position(node1, node2)
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
      use t_geometry_data
!
      implicit none
!
      type(node_data), intent(in) :: node1, node2
!
      integer(kind = kint) :: compare_field_vector
      external :: compare_field_vector
!
      character(len=kchara), parameter :: field_name = 'position'
      integer(kind = kint) :: iflag
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'numnod', node1%numnod, node2%numnod
      end if
!
      iflag = 0
      if(node1%numnod .ne. node2%numnod) then
        write(*,*) 'Number of node is different',                       &
     &             node1%numnod, node2%numnod
        iflag = iflag + 1
      end if
      iflag = iflag                                                     &
     &     + compare_field_vector(node1%numnod, n_vector, field_name,   &
     &                            node1%xx(1,1), node2%xx(1,1))
      compare_node_position = iflag
!
      end function compare_node_position
!
!  --------------------------------------------------------------------
!
      integer(kind = kint) function compare_ele_connect(ele1, ele2)
!
      use m_precision
      use m_machine_parameter
      use t_geometry_data
!
      implicit none
!
      type(element_data), intent(in) :: ele1, ele2
!
      integer(kind = kint) :: iele, k1
      integer(kind = kint) :: iflag
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'numele', ele1%numele, ele2%numele
        write(*,*) 'nnod_4_ele', ele1%nnod_4_ele, ele2%nnod_4_ele
      end if
!
      iflag = 0
      if(ele1%numele .ne. ele2%numele) then
        write(*,*) 'Number of element is differenct: ',                 &
     &             ele1%numele, ele2%numele
        iflag = iflag + 1
      end if
      if(ele1%nnod_4_ele .ne. ele2%nnod_4_ele) then
        write(*,*) 'Element type is differennt: ',                      &
     &             ele1%nnod_4_ele, ele2%nnod_4_ele
        iflag = iflag + 1
      end if
!
      do iele = 1, ele1%numele
        do k1 = 1, ele1%nnod_4_ele
          if(ele1%ie(iele,k1) .ne. ele2%ie(iele,k1)) then
            write(*,*) 'connectivity at ', iele, k1, ' is differ',      &
     &          ele1%ie(iele,k1), ele2%ie(iele,k1)
            iflag = iflag + 1
          end if
        end do
      end do
      compare_ele_connect = iflag
!
      end function compare_ele_connect
!
!  --------------------------------------------------------------------
!
      integer(kind = kint) function compare_field_data(fld1, fld2)
!
      use m_precision
      use m_machine_parameter
      use t_phys_data
!
      implicit none
!
      type(phys_data), intent(in) :: fld1, fld2
!
      integer(kind = kint) :: ifld, ist
      integer(kind = kint) :: iflag
!
      integer(kind = kint) :: compare_field_vector
      external :: compare_field_vector
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'n_point', fld1%n_point, fld2%n_point
        write(*,*) 'num_phys', fld1%num_phys, fld2%num_phys
        write(*,*) 'ntot_phys', fld1%ntot_phys, fld2%ntot_phys
      end if
!
      iflag = 0
      if(fld1%n_point .ne. fld2%n_point) then
        write(*,*) 'Number of point in field data is different',        &
     &             fld1%n_point, fld2%n_point
        iflag = iflag + 1
      end if
      if(fld1%num_phys .ne. fld2%num_phys) then
        write(*,*) 'Number of field is different',                      &
     &            fld1%num_phys, fld2%num_phys
        iflag = iflag + 1
      end if
      if(fld1%ntot_phys .ne. fld2%ntot_phys) then
        write(*,*) 'Number of total components is different',           &
     &            fld1%ntot_phys, fld2%ntot_phys
        iflag = iflag + 1
      end if
!
      do ifld = 1, fld1%num_phys
        if(fld1%phys_name(ifld) .ne. fld2%phys_name(ifld)) then
          write(*,*) 'field name at ', ifld, ' is different: ',         &
     &      trim(fld1%phys_name(ifld)), ' ', trim(fld2%phys_name(ifld))
          iflag = iflag + 1
        end if
      end do
      do ifld = 1, fld1%num_phys
        if(fld1%num_component(ifld) .ne. fld2%num_component(ifld)) then
          write(*,*) 'number of component at ', ifld, ' is different:', &
     &        fld1%num_component(ifld),  fld2%num_component(ifld)
          iflag = iflag + 1
        end if
      end do
      do ifld = 1, fld1%num_phys
        if(fld1%istack_component(ifld)                                  &
     &        .ne. fld2%istack_component(ifld)) then
          write(*,*) 'field stack at ', ifld, ' is different:',         &
     &        fld1%istack_component(ifld), fld2%istack_component(ifld)
          iflag = iflag + 1
        end if
      end do
!
      do ifld = 1, fld1%num_phys
        ist = fld2%istack_component(ifld-1)
        iflag = iflag + compare_field_vector(fld1%n_point,              &
     &                  fld1%num_component(ifld), fld1%phys_name(ifld), &
     &                  fld1%d_fld(1,ist+1), fld2%d_fld(1,ist+1))
      end do
!
      compare_field_data = iflag
!
      end function compare_field_data
!
!  --------------------------------------------------------------------
!
      integer(kind = kint) function compare_field_vector                &
     &      (n_point, numdir, fld_name, d_fld1, d_fld2)
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      character(len=kchara), intent(in) :: fld_name
      integer(kind = kint), intent(in) :: n_point, numdir
      real(kind = kreal), intent(in) :: d_fld1(n_point,numdir)
      real(kind = kreal), intent(in) :: d_fld2(n_point,numdir)
!
      real(kind = kreal), allocatable :: vmin(:), vmax(:), size(:)
      real(kind = kreal) :: scale, diff
      integer(kind = kint) :: inod, ifld, icomp
      integer(kind = kint) :: iflag
!
      real(kind = kreal), parameter :: TINY = 1.0d-12
!
      iflag = 0
      allocate(vmin(numdir))
      allocate(vmax(numdir))
      allocate(size(numdir))
!
!$omp parallel workshare
      vmin(1:numdir) = minval(d_fld1(1:n_point,1:numdir),1)
      vmax(1:numdir) = maxval(d_fld1(1:n_point,1:numdir),1)
!$omp end parallel workshare
      size(1:numdir) = vmax(1:numdir) - vmin(1:numdir)
!
      scale = 0.0d0
      do icomp = 1, numdir
        scale = scale + vmax(icomp)**2
      end do
      scale = sqrt(scale)
      if(iflag_debug .gt. 0) write(*,*) ifld, 'scale for ',             &
     &                                 trim(fld_name), ': ', scale
!
      do inod = 1, n_point
        do icomp = 1, numdir
          diff = d_fld2(inod,icomp) - d_fld1(inod,icomp)
          if((abs(diff) / scale) .gt. TINY) then
            write(*,*) icomp, '-component of ', trim(fld_name),     &
     &                ' at ', inod, ' is different: ', diff
            iflag = iflag + 1
          end if
        end do
      end do
      deallocate(vmin, vmax, size)
      compare_field_vector = iflag
!
      end function compare_field_vector
!
!  --------------------------------------------------------------------
!
