!
!      module int_region_ele_field_2_node
!
!     Written by H. Matsui on Oct., 2006
!
!      subroutine int_fl_ele_scalar_2_node(scalar_nod, scalar_ele)
!      subroutine int_fl_ele_vector_2_node(vector_nod, vector_ele)
!
!      subroutine int_cd_ele_scalar_2_node(scalar_nod, scalar_ele)
!      subroutine int_cd_ele_vector_2_node(vector_nod, vector_ele)
!
!      subroutine int_ins_ele_scalar_2_node(scalar_nod, scalar_ele)
!      subroutine int_ins_ele_vector_2_node(vector_nod, vector_ele)
!
      module int_region_ele_field_2_node
!
      use m_precision
      use m_constants
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
!
      use int_element_field_2_node
      use cal_ff_smp_to_ffs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_fl_ele_scalar_2_node(scalar_nod, scalar_ele)
!
!
      real(kind = kreal), intent(in) :: scalar_ele(ele1%numele)
      real(kind = kreal), intent(inout) :: scalar_nod(node1%numnod)
!
!
      call int_area_ele_scalar_2_node(iele_fl_smp_stack, scalar_ele)
      call cal_ff_smp_2_scalar(node1, rhs_tbl1,                         &
     &    ff_smp, ml_fl, n_scalar, ione, scalar_nod)
!
      end subroutine int_fl_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_fl_ele_vector_2_node(vector_nod, vector_ele)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: vector_ele(ele1%numele,n_vector)
      real(kind = kreal), intent(inout)                                 &
     &                   :: vector_nod(node1%numnod,n_vector)
!
!
      call int_area_ele_vector_2_node(iele_fl_smp_stack, vector_ele)
      call cal_ff_smp_2_vector(node1, rhs_tbl1,                         &
     &    ff_smp, ml_fl, n_vector, ione, vector_nod)
!
      end subroutine int_fl_ele_vector_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_cd_ele_scalar_2_node(scalar_nod, scalar_ele)
!
      real(kind = kreal), intent(in) :: scalar_ele(ele1%numele)
      real(kind = kreal), intent(inout) :: scalar_nod(node1%numnod)
!
!
      call int_area_ele_scalar_2_node(iele_cd_smp_stack, scalar_ele)
      call cal_ff_smp_2_scalar(node1, rhs_tbl1,                         &
     &    ff_smp, ml_cd, n_scalar, ione, scalar_nod)
!
      end subroutine int_cd_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_cd_ele_vector_2_node(vector_nod, vector_ele)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: vector_ele(ele1%numele,n_vector)
      real(kind = kreal), intent(inout)                                 &
     &                   :: vector_nod(node1%numnod,n_vector)
!
!
      call int_area_ele_vector_2_node(iele_cd_smp_stack, vector_ele)
      call cal_ff_smp_2_vector(node1, rhs_tbl1,                         &
     &    ff_smp, ml_cd, n_vector, ione, vector_nod)
!
      end subroutine int_cd_ele_vector_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_ins_ele_scalar_2_node(scalar_nod, scalar_ele)
!
      real(kind = kreal), intent(in) :: scalar_ele(ele1%numele)
      real(kind = kreal), intent(inout) :: scalar_nod(node1%numnod)
!
!
      call int_area_ele_scalar_2_node(iele_ins_smp_stack, scalar_ele)
      call cal_ff_smp_2_scalar(node1, rhs_tbl1,                         &
     &    ff_smp, ml_ins, n_scalar, ione, scalar_nod)
!
      end subroutine int_ins_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_ins_ele_vector_2_node(vector_nod, vector_ele)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: vector_ele(ele1%numele,n_vector)
      real(kind = kreal), intent(inout)                                 &
     &                   :: vector_nod(node1%numnod,n_vector)
!
!
      call int_area_ele_vector_2_node(iele_ins_smp_stack, vector_ele)
      call cal_ff_smp_2_vector(node1, rhs_tbl1,                         &
     &    ff_smp, ml_ins, n_vector, ione, vector_nod)
!
      end subroutine int_ins_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      end module int_region_ele_field_2_node
